import { Compartment, EditorState, Prec } from "@codemirror/state";
import {
  EditorView,
  crosshairCursor,
  drawSelection,
  dropCursor,
  highlightActiveLine,
  highlightActiveLineGutter,
  highlightSpecialChars,
  keymap,
  lineNumbers,
  placeholder,
  rectangularSelection,
} from "@codemirror/view";
import {
  defaultKeymap,
  history,
  historyKeymap,
  indentWithTab,
  redo,
  redoDepth,
  undo,
  undoDepth,
} from "@codemirror/commands";
import {
  bracketMatching,
  defaultHighlightStyle,
  indentOnInput,
  syntaxHighlighting,
} from "@codemirror/language";
import { markdown } from "@codemirror/lang-markdown";
import {
  SearchQuery,
  closeSearchPanel,
  findNext,
  findPrevious,
  highlightSelectionMatches,
  openSearchPanel,
  replaceAll,
  replaceNext,
  search,
  searchPanelOpen,
  setSearchQuery,
} from "@codemirror/search";

const MAX_MATCH_COUNT_SCAN = 10000;

function computeEditorStats(state) {
  const doc = state.doc;
  const head = state.selection.main.head;
  const line = doc.lineAt(head);
  return {
    lineCount: doc.lines,
    charCount: doc.length,
    cursorLine: line.number,
    cursorCol: head - line.from + 1,
  };
}

function buildSearchQueryObject(params) {
  return new SearchQuery({
    search: params.query || "",
    replace: params.replacement || "",
    caseSensitive: Boolean(params.caseSensitive),
    regexp: Boolean(params.useRegex),
  });
}

function computeSearchStatus(state, params) {
  if (!params || !params.query) {
    return { matchCount: 0, activeMatch: 0, regexError: null };
  }
  const sq = buildSearchQueryObject(params);
  if (!sq.valid) {
    return {
      matchCount: 0,
      activeMatch: 0,
      regexError: "Invalid regular expression",
    };
  }
  let cursor;
  try {
    cursor = sq.getCursor(state.doc);
  } catch (err) {
    const detail = err && err.message ? String(err.message) : "Invalid regular expression";
    return { matchCount: 0, activeMatch: 0, regexError: detail };
  }

  const selFrom = state.selection.main.from;
  const selTo = state.selection.main.to;
  let matchCount = 0;
  let activeMatch = 0;

  let next = cursor.next();
  while (!next.done && matchCount < MAX_MATCH_COUNT_SCAN) {
    matchCount += 1;
    const { from, to } = next.value;
    if (activeMatch === 0 && from === selFrom && to === selTo) {
      activeMatch = matchCount;
    }
    next = cursor.next();
  }
  return { matchCount, activeMatch, regexError: null };
}

class TamaTsuzuraEditorManager {
  constructor() {
    this.view = null;
    this.hostElement = null;
    this.listeners = new Set();
    this.currentDocId = null;
    this.currentSyncVersion = -1;
    this.currentFormat = "markdown";
    this.currentReadOnly = false;
    this.currentPlaceholder = "";
    this.pendingSync = null;
    this.pendingFocus = false;
    this.activeSearch = null;
    this.isApplyingRemote = false;

    this.languageCompartment = new Compartment();
    this.readOnlyCompartment = new Compartment();
    this.editableCompartment = new Compartment();
    this.placeholderCompartment = new Compartment();

    this.handleGlobalKeyDown = this.handleGlobalKeyDown.bind(this);
    if (typeof window !== "undefined") {
      window.addEventListener("keydown", this.handleGlobalKeyDown, { capture: true });
    }
  }

  // Capture Ctrl/Cmd+S, Ctrl/Cmd+F, and Ctrl+H in a single place so shortcuts
  // work both inside and outside CodeMirror without double-firing.
  handleGlobalKeyDown(event) {
    if (!(event.ctrlKey || event.metaKey) || event.altKey) {
      return;
    }
    const key = (event.key || "").toLowerCase();
    if (key === "s") {
      event.preventDefault();
      event.stopPropagation();
      this.emit({ type: "saveShortcut" });
    } else if (key === "f") {
      if (this.view) {
        event.preventDefault();
        event.stopPropagation();
        this.emit({ type: "openSearch", showReplace: false });
      }
    } else if (key === "h" && !event.metaKey) {
      if (this.view) {
        event.preventDefault();
        event.stopPropagation();
        this.emit({ type: "openSearch", showReplace: true });
      }
    }
  }

  subscribe(callback) {
    if (typeof callback !== "function") {
      return () => {};
    }
    this.listeners.add(callback);
    if (this.view) {
      this.emitHistory(this.view.state);
      if (this.activeSearch) {
        this.emitSearchStatus(this.view.state);
      }
    }
    return () => {
      this.listeners.delete(callback);
    };
  }

  emit(payload) {
    const serialized = JSON.stringify(payload);
    for (const listener of this.listeners) {
      try {
        listener(serialized);
      } catch (err) {
        console.error("[tamatsuzura] Editor listener failed:", err);
      }
    }
  }

  emitHistory(state) {
    this.emit({
      type: "historyState",
      canUndo: undoDepth(state) > 0,
      canRedo: redoDepth(state) > 0,
    });
  }

  emitSearchStatus(state) {
    const status = computeSearchStatus(state, this.activeSearch);
    this.emit({
      type: "searchStatus",
      matchCount: status.matchCount,
      activeMatch: status.activeMatch,
      regexError: status.regexError,
    });
  }

  hasValidActiveSearch() {
    if (!this.activeSearch || !this.activeSearch.query) {
      return false;
    }
    return buildSearchQueryObject(this.activeSearch).valid;
  }

  buildExtensions(format, readOnly, placeholderText) {
    const self = this;
    const editorTheme = EditorView.theme({
      "&": {
        height: "100%",
        fontSize: "16px",
        backgroundColor: "#ffffff",
      },
      ".cm-scroller": {
        overflow: "auto",
        fontFamily:
          '"SFMono-Regular", Menlo, Monaco, Consolas, "Liberation Mono", "Noto Sans Mono CJK JP", monospace',
        lineHeight: "1.7",
      },
      ".cm-content": {
        padding: "12px 10px 64px",
        caretColor: "#1f5f99",
      },
      ".cm-gutters": {
        backgroundColor: "#f8fafc",
        color: "#64748b",
        borderRight: "1px solid #e2e8f0",
      },
      ".cm-activeLine": {
        backgroundColor: "rgba(31, 95, 153, 0.05)",
      },
      ".cm-activeLineGutter": {
        backgroundColor: "rgba(31, 95, 153, 0.09)",
      },
      ".cm-searchMatch": {
        backgroundColor: "#fde68a",
        outline: "1px solid #d97706",
      },
      ".cm-searchMatch.cm-searchMatch-selected": {
        backgroundColor: "#f59e0b",
        color: "#ffffff",
      },
      "&.cm-focused": {
        outline: "none",
      },
    });

    // Prevent CodeMirror's built-in search/save keybindings from conflicting;
    // actual shortcut events are emitted exclusively by handleGlobalKeyDown.
    const guardKeymap = Prec.highest(
      keymap.of([
        { key: "Mod-s", preventDefault: true, run: () => true },
        { key: "Mod-f", preventDefault: true, run: () => true },
        { key: "Ctrl-h", preventDefault: true, run: () => true },
      ])
    );

    const updateListener = EditorView.updateListener.of((update) => {
      if (self.isApplyingRemote) {
        return;
      }
      if (update.docChanged) {
        const nextBody = update.state.doc.toString();
        if (self.pendingSync && self.pendingSync.docId === self.currentDocId) {
          self.pendingSync = { ...self.pendingSync, body: nextBody };
        }
        const stats = computeEditorStats(update.state);
        self.emit({
          type: "bodyChanged",
          body: nextBody,
          stats,
        });
        self.emitHistory(update.state);
        if (self.activeSearch) {
          self.emitSearchStatus(update.state);
        }
      } else if (update.selectionSet) {
        const stats = computeEditorStats(update.state);
        self.emit({
          type: "cursorMoved",
          stats,
        });
        if (self.activeSearch) {
          self.emitSearchStatus(update.state);
        }
      }
    });

    const langExt = format === "markdown" ? markdown() : [];

    return [
      lineNumbers(),
      highlightActiveLineGutter(),
      highlightSpecialChars(),
      history(),
      drawSelection(),
      dropCursor(),
      EditorState.allowMultipleSelections.of(true),
      indentOnInput(),
      syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
      bracketMatching(),
      rectangularSelection(),
      crosshairCursor(),
      highlightActiveLine(),
      highlightSelectionMatches(),
      EditorView.lineWrapping,
      search({
        createPanel: () => {
          const dummy = document.createElement("div");
          dummy.style.display = "none";
          return { dom: dummy };
        },
      }),
      editorTheme,
      this.languageCompartment.of(langExt),
      this.readOnlyCompartment.of(EditorState.readOnly.of(Boolean(readOnly))),
      this.editableCompartment.of(EditorView.editable.of(!readOnly)),
      this.placeholderCompartment.of(placeholderText ? placeholder(placeholderText) : []),
      guardKeymap,
      keymap.of([...defaultKeymap, ...historyKeymap, indentWithTab]),
      updateListener,
    ];
  }

  mount(hostElement) {
    if (this.view && this.hostElement === hostElement) {
      return;
    }
    this.unmount(this.hostElement);

    const container = document.createElement("div");
    container.className = "tt-cm-inner";
    hostElement.textContent = "";
    hostElement.appendChild(container);
    this.hostElement = hostElement;

    const initial = this.pendingSync || {
      docId: this.currentDocId || "",
      version: Math.max(0, this.currentSyncVersion),
      body: "",
      format: this.currentFormat,
      readOnly: this.currentReadOnly,
      placeholder: this.currentPlaceholder,
    };

    const state = EditorState.create({
      doc: initial.body || "",
      extensions: this.buildExtensions(initial.format, initial.readOnly, initial.placeholder),
    });

    this.view = new EditorView({
      state,
      parent: container,
    });

    this.currentDocId = initial.docId;
    this.currentSyncVersion = initial.version;
    this.currentFormat = initial.format;
    this.currentReadOnly = Boolean(initial.readOnly);
    this.currentPlaceholder = initial.placeholder || "";

    if (this.activeSearch) {
      this.applySearchState(this.activeSearch);
    }
    this.emit({
      type: "cursorMoved",
      stats: computeEditorStats(this.view.state),
    });
    this.emitHistory(this.view.state);

    if (this.pendingFocus) {
      this.pendingFocus = false;
      this.view.focus();
    }
  }

  unmount(hostElement) {
    if (hostElement && this.hostElement && hostElement !== this.hostElement) {
      return;
    }
    if (this.view) {
      this.view.destroy();
      this.view = null;
    }
    this.hostElement = null;
  }

  resetEditor() {
    this.pendingSync = null;
    this.pendingFocus = false;
    this.currentDocId = null;
    this.currentSyncVersion = -1;
    this.activeSearch = null;
    if (this.view) {
      const nextState = EditorState.create({
        doc: "",
        extensions: this.buildExtensions("markdown", false, this.currentPlaceholder),
      });
      this.isApplyingRemote = true;
      try {
        this.view.setState(nextState);
      } finally {
        this.isApplyingRemote = false;
      }
      this.emitHistory(this.view.state);
    }
  }

  syncDocument(payload) {
    this.pendingSync = payload;
    if (!this.view) {
      this.currentDocId = payload.docId;
      this.currentSyncVersion = payload.version;
      this.currentFormat = payload.format;
      this.currentReadOnly = Boolean(payload.readOnly);
      this.currentPlaceholder = payload.placeholder || "";
      return;
    }

    const switchingDocument = this.currentDocId !== payload.docId;
    const mustResetState = switchingDocument || Boolean(payload.resetHistory);
    if (mustResetState) {
      const nextState = EditorState.create({
        doc: payload.body || "",
        extensions: this.buildExtensions(payload.format, payload.readOnly, payload.placeholder),
      });
      this.isApplyingRemote = true;
      try {
        this.view.setState(nextState);
        this.currentDocId = payload.docId;
        this.currentSyncVersion = payload.version;
        this.currentFormat = payload.format;
        this.currentReadOnly = Boolean(payload.readOnly);
        this.currentPlaceholder = payload.placeholder || "";
      } finally {
        this.isApplyingRemote = false;
      }
      if (this.activeSearch) {
        this.applySearchState(this.activeSearch);
      }
      this.emit({
        type: "cursorMoved",
        stats: computeEditorStats(this.view.state),
      });
      this.emitHistory(this.view.state);
      return;
    }

    const effects = [];
    if (this.currentFormat !== payload.format) {
      effects.push(
        this.languageCompartment.reconfigure(payload.format === "markdown" ? markdown() : [])
      );
      this.currentFormat = payload.format;
    }
    if (this.currentReadOnly !== Boolean(payload.readOnly)) {
      const ro = Boolean(payload.readOnly);
      effects.push(this.readOnlyCompartment.reconfigure(EditorState.readOnly.of(ro)));
      effects.push(this.editableCompartment.reconfigure(EditorView.editable.of(!ro)));
      this.currentReadOnly = ro;
    }
    if (this.currentPlaceholder !== (payload.placeholder || "")) {
      const nextPh = payload.placeholder || "";
      effects.push(this.placeholderCompartment.reconfigure(nextPh ? placeholder(nextPh) : []));
      this.currentPlaceholder = nextPh;
    }

    const versionChanged = payload.version !== this.currentSyncVersion;
    const currentText = this.view.state.doc.toString();
    const shouldReplaceBody = versionChanged && currentText !== payload.body;
    this.currentSyncVersion = payload.version;

    if (shouldReplaceBody || effects.length > 0) {
      const spec = {};
      if (shouldReplaceBody) {
        spec.changes = { from: 0, to: currentText.length, insert: payload.body };
      }
      if (effects.length > 0) {
        spec.effects = effects;
      }
      this.isApplyingRemote = true;
      try {
        this.view.dispatch(spec);
      } finally {
        this.isApplyingRemote = false;
      }
      if (shouldReplaceBody) {
        this.emit({
          type: "cursorMoved",
          stats: computeEditorStats(this.view.state),
        });
        this.emitHistory(this.view.state);
        if (this.activeSearch) {
          this.emitSearchStatus(this.view.state);
        }
      }
    }
  }

  applySearchState(params) {
    this.activeSearch = params;
    if (!this.view) {
      return;
    }
    const sq = params && params.query ? buildSearchQueryObject(params) : null;
    if (sq && sq.valid) {
      // @codemirror/search only renders cm-searchMatch decorations while its internal search panel is open.
      // Open the hidden dummy panel first (since openSearchPanel resets query to default), then setSearchQuery.
      if (!searchPanelOpen(this.view.state)) {
        openSearchPanel(this.view);
      }
      this.view.dispatch({ effects: setSearchQuery.of(sq) });
    } else {
      this.view.dispatch({ effects: setSearchQuery.of(new SearchQuery({ search: "" })) });
      if (searchPanelOpen(this.view.state)) {
        closeSearchPanel(this.view);
      }
    }
    this.emitSearchStatus(this.view.state);
  }

  dispatchCommand(commandJson) {
    let cmd;
    try {
      cmd = JSON.parse(commandJson);
    } catch (err) {
      console.error("[tamatsuzura] Invalid editor command JSON:", err);
      return;
    }
    if (!cmd || typeof cmd.type !== "string") {
      return;
    }

    switch (cmd.type) {
      case "syncDocument":
        this.syncDocument(cmd);
        break;
      case "resetEditor":
        this.resetEditor();
        break;
      case "applySearch":
        this.applySearchState({
          query: cmd.query || "",
          replacement: cmd.replacement || "",
          caseSensitive: Boolean(cmd.caseSensitive),
          useRegex: Boolean(cmd.useRegex),
        });
        break;
      case "clearSearch":
        this.applySearchState(null);
        break;
      case "findNext":
        if (this.view && this.hasValidActiveSearch()) {
          findNext(this.view);
          this.emitSearchStatus(this.view.state);
        }
        break;
      case "findPrevious":
        if (this.view && this.hasValidActiveSearch()) {
          findPrevious(this.view);
          this.emitSearchStatus(this.view.state);
        }
        break;
      case "replaceNext":
        if (this.view && !this.currentReadOnly && this.hasValidActiveSearch()) {
          replaceNext(this.view);
          this.emitSearchStatus(this.view.state);
        }
        break;
      case "replaceAll":
        if (this.view && !this.currentReadOnly && this.hasValidActiveSearch()) {
          replaceAll(this.view);
          this.emitSearchStatus(this.view.state);
        }
        break;
      case "undo":
        if (this.view && !this.currentReadOnly) {
          undo(this.view);
          this.emitHistory(this.view.state);
        }
        break;
      case "redo":
        if (this.view && !this.currentReadOnly) {
          redo(this.view);
          this.emitHistory(this.view.state);
        }
        break;
      case "focus":
        if (this.view) {
          this.view.focus();
        } else {
          this.pendingFocus = true;
        }
        break;
      default:
        console.warn("[tamatsuzura] Unknown editor command:", cmd.type);
    }
  }
}

const manager = new TamaTsuzuraEditorManager();
globalThis.__tamatsuzuraEditor = manager;

if (typeof customElements !== "undefined" && !customElements.get("tt-codemirror-editor")) {
  class TamaTsuzuraCodeMirrorElement extends HTMLElement {
    connectedCallback() {
      manager.mount(this);
    }

    disconnectedCallback() {
      manager.unmount(this);
    }
  }
  customElements.define("tt-codemirror-editor", TamaTsuzuraCodeMirrorElement);
}
