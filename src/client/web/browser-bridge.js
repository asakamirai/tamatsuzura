// Thin browser bridge for IndexedDB draft autosave/recovery and local file export/import.
// Keeps browser-specific DOM / IndexedDB / Blob APIs isolated from Haskell (Miso) core logic.

const DB_NAME = "tamatsuzura";
const DB_VERSION = 1;
const DRAFTS_STORE = "drafts";

class TamaTsuzuraDraftStorageBridge {
  constructor() {
    this.dbPromise = null;
    this.listeners = new Set();
    this.pendingTimers = new Map();
    this.pendingRecords = new Map();

    this.handleLifecycleFlush = this.handleLifecycleFlush.bind(this);
    this.handleVisibilityChange = this.handleVisibilityChange.bind(this);

    if (typeof window !== "undefined") {
      window.addEventListener("pagehide", this.handleLifecycleFlush);
      window.addEventListener("beforeunload", this.handleLifecycleFlush);
    }
    if (typeof document !== "undefined") {
      document.addEventListener("visibilitychange", this.handleVisibilityChange);
    }
  }

  handleLifecycleFlush() {
    this.flushPending();
  }

  handleVisibilityChange() {
    if (typeof document !== "undefined" && document.visibilityState === "hidden") {
      this.flushPending();
    }
  }

  subscribe(callback) {
    if (typeof callback !== "function") {
      return () => {};
    }
    this.listeners.add(callback);
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
        console.error("[tamatsuzura] Storage listener failed:", err);
      }
    }
  }

  openDb() {
    if (this.dbPromise) {
      return this.dbPromise;
    }
    if (typeof indexedDB === "undefined") {
      return Promise.reject(new Error("IndexedDB is not available in this browser"));
    }

    this.dbPromise = new Promise((resolve, reject) => {
      let request;
      try {
        request = indexedDB.open(DB_NAME, DB_VERSION);
      } catch (err) {
        this.dbPromise = null;
        reject(err);
        return;
      }

      request.onupgradeneeded = () => {
        const db = request.result;
        if (!db.objectStoreNames.contains(DRAFTS_STORE)) {
          db.createObjectStore(DRAFTS_STORE, { keyPath: "docId" });
        }
      };

      request.onsuccess = () => {
        const db = request.result;
        db.onversionchange = () => {
          db.close();
          this.dbPromise = null;
        };
        db.onclose = () => {
          this.dbPromise = null;
        };
        resolve(db);
      };

      request.onerror = () => {
        this.dbPromise = null;
        reject(request.error || new Error("Failed to open IndexedDB"));
      };

      request.onblocked = () => {
        this.dbPromise = null;
        reject(new Error("IndexedDB open was blocked by another tab"));
      };
    });

    return this.dbPromise;
  }

  cancelPending(docId) {
    const timer = this.pendingTimers.get(docId);
    if (timer !== undefined) {
      clearTimeout(timer);
      this.pendingTimers.delete(docId);
    }
    this.pendingRecords.delete(docId);
  }

  writeRecord(record) {
    return this.openDb().then(
      (db) =>
        new Promise((resolve, reject) => {
          const tx = db.transaction(DRAFTS_STORE, "readwrite");
          const store = tx.objectStore(DRAFTS_STORE);
          store.put(record);
          tx.oncomplete = () => resolve();
          tx.onerror = () => reject(tx.error || new Error("IndexedDB write failed"));
          tx.onabort = () => reject(tx.error || new Error("IndexedDB write aborted"));
        })
    );
  }

  scheduleSave(draftJson, debounceMs) {
    let record;
    try {
      record = JSON.parse(draftJson);
    } catch (err) {
      const msg = err && err.message ? String(err.message) : String(err);
      console.error("[tamatsuzura] Invalid draft JSON:", err);
      this.emit({ type: "draftSaveFailed", docId: "", error: msg });
      return;
    }

    if (!record || typeof record.docId !== "string" || record.docId.length === 0) {
      return;
    }

    const docId = record.docId;
    this.cancelPending(docId);
    this.pendingRecords.set(docId, record);

    const delay = typeof debounceMs === "number" && debounceMs > 0 ? debounceMs : 0;
    const executeWrite = () => {
      this.pendingTimers.delete(docId);
      const latest = this.pendingRecords.get(docId);
      if (!latest) {
        return;
      }
      this.pendingRecords.delete(docId);
      this.writeRecord(latest)
        .then(() => {
          // Only emit saved if no newer draft superseded this docId while writing
          if (!this.pendingRecords.has(docId)) {
            this.emit({
              type: "draftSaved",
              docId,
              savedAt: latest.savedAt,
            });
          }
        })
        .catch((err) => {
          const msg = err && err.message ? String(err.message) : String(err);
          console.error("[tamatsuzura] Failed to persist draft:", err);
          this.emit({
            type: "draftSaveFailed",
            docId,
            error: msg,
          });
        });
    };

    if (delay === 0) {
      executeWrite();
    } else {
      const timer = setTimeout(executeWrite, delay);
      this.pendingTimers.set(docId, timer);
    }
  }

  flushPending() {
    if (this.pendingRecords.size === 0) {
      return;
    }
    const records = Array.from(this.pendingRecords.values());
    for (const timer of this.pendingTimers.values()) {
      clearTimeout(timer);
    }
    this.pendingTimers.clear();
    this.pendingRecords.clear();

    const emitFlushFailed = (err) => {
      const msg = err && err.message ? String(err.message) : String(err);
      console.error("[tamatsuzura] Failed to flush pending drafts:", err);
      for (const record of records) {
        if (!this.pendingRecords.has(record.docId)) {
          this.emit({
            type: "draftSaveFailed",
            docId: record.docId,
            error: msg,
          });
        }
      }
    };

    this.openDb()
      .then((db) => {
        const tx = db.transaction(DRAFTS_STORE, "readwrite");
        const store = tx.objectStore(DRAFTS_STORE);
        for (const record of records) {
          store.put(record);
        }
        tx.oncomplete = () => {
          for (const record of records) {
            if (!this.pendingRecords.has(record.docId)) {
              this.emit({
                type: "draftSaved",
                docId: record.docId,
                savedAt: record.savedAt,
              });
            }
          }
        };
        tx.onerror = () => emitFlushFailed(tx.error || new Error("IndexedDB flush failed"));
        tx.onabort = () => emitFlushFailed(tx.error || new Error("IndexedDB flush aborted"));
      })
      .catch(emitFlushFailed);
  }

  deleteDraft(docId) {
    if (typeof docId !== "string" || docId.length === 0) {
      return Promise.resolve();
    }
    this.cancelPending(docId);
    return this.openDb()
      .then(
        (db) =>
          new Promise((resolve, reject) => {
            const tx = db.transaction(DRAFTS_STORE, "readwrite");
            const store = tx.objectStore(DRAFTS_STORE);
            store.delete(docId);
            tx.oncomplete = () => resolve();
            tx.onerror = () => reject(tx.error || new Error("IndexedDB delete failed"));
            tx.onabort = () => reject(tx.error || new Error("IndexedDB delete aborted"));
          })
      )
      .catch((err) => {
        console.error("[tamatsuzura] Failed to delete draft:", err);
      });
  }

  async listDrafts() {
    try {
      const db = await this.openDb();
      const drafts = await new Promise((resolve, reject) => {
        const tx = db.transaction(DRAFTS_STORE, "readonly");
        const store = tx.objectStore(DRAFTS_STORE);
        const req = store.getAll();
        req.onsuccess = () => resolve(Array.isArray(req.result) ? req.result : []);
        req.onerror = () => reject(req.error || new Error("IndexedDB read failed"));
      });
      return JSON.stringify({ ok: true, drafts });
    } catch (err) {
      const msg = err && err.message ? String(err.message) : String(err);
      return JSON.stringify({ ok: false, error: msg });
    }
  }
}

class TamaTsuzuraFileTransferBridge {
  exportTextFile(fileName, mimeType, content) {
    try {
      if (typeof Blob === "undefined" || typeof URL === "undefined" || typeof document === "undefined") {
        return JSON.stringify({ ok: false, error: "Browser download API is unavailable" });
      }
      const blob = new Blob([content], { type: mimeType || "text/plain;charset=utf-8" });
      const objectUrl = URL.createObjectURL(blob);
      const anchor = document.createElement("a");
      anchor.href = objectUrl;
      anchor.download = fileName || "untitled.txt";
      anchor.style.display = "none";
      document.body.appendChild(anchor);
      anchor.click();
      anchor.remove();
      setTimeout(() => {
        try {
          URL.revokeObjectURL(objectUrl);
        } catch (_err) {
          // Ignore cleanup failure
        }
      }, 1000);
      return JSON.stringify({ ok: true });
    } catch (err) {
      const msg = err && err.message ? String(err.message) : String(err);
      return JSON.stringify({ ok: false, error: msg });
    }
  }

  pickTextFile(accept, maxBytes) {
    if (typeof document === "undefined") {
      return Promise.resolve(
        JSON.stringify({
          status: "error",
          code: "read_failed",
          message: "Browser file picker is unavailable",
        })
      );
    }

    return new Promise((resolve) => {
      let settled = false;
      let focusTimer = null;
      const input = document.createElement("input");
      input.type = "file";
      if (typeof accept === "string" && accept.length > 0) {
        input.accept = accept;
      }
      input.style.position = "fixed";
      input.style.left = "-9999px";
      input.style.opacity = "0";
      input.tabIndex = -1;

      const finish = (payload) => {
        if (settled) {
          return;
        }
        settled = true;
        if (focusTimer !== null) {
          clearTimeout(focusTimer);
          focusTimer = null;
        }
        window.removeEventListener("focus", handleWindowFocus);
        input.remove();
        resolve(JSON.stringify(payload));
      };

      const handleWindowFocus = () => {
        // Fallback cancel detection for browsers without input "cancel" event
        focusTimer = setTimeout(() => {
          focusTimer = null;
          if (!settled && (!input.files || input.files.length === 0)) {
            finish({ status: "cancelled" });
          }
        }, 350);
      };

      input.addEventListener("cancel", () => {
        finish({ status: "cancelled" });
      });

      input.addEventListener("change", async () => {
        const file = input.files && input.files[0];
        if (!file) {
          finish({ status: "cancelled" });
          return;
        }

        if (typeof maxBytes === "number" && maxBytes > 0 && file.size > maxBytes) {
          finish({
            status: "error",
            code: "too_large",
            actualBytes: file.size,
            maxBytes,
          });
          return;
        }

        try {
          const buffer = await file.arrayBuffer();
          if (typeof maxBytes === "number" && maxBytes > 0 && buffer.byteLength > maxBytes) {
            finish({
              status: "error",
              code: "too_large",
              actualBytes: buffer.byteLength,
              maxBytes,
            });
            return;
          }
          const decoder = new TextDecoder("utf-8", { fatal: true });
          const content = decoder.decode(buffer);
          finish({
            status: "selected",
            fileName: file.name || "",
            content,
          });
        } catch (err) {
          if (err instanceof TypeError) {
            finish({
              status: "error",
              code: "invalid_utf8",
              message: err.message || "Invalid UTF-8 byte sequence",
            });
          } else {
            const msg = err && err.message ? String(err.message) : String(err);
            finish({
              status: "error",
              code: "read_failed",
              message: msg,
            });
          }
        }
      });

      document.body.appendChild(input);
      window.addEventListener("focus", handleWindowFocus, { once: true });
      try {
        input.click();
      } catch (err) {
        const msg = err && err.message ? String(err.message) : String(err);
        finish({
          status: "error",
          code: "read_failed",
          message: msg,
        });
      }
    });
  }
}

globalThis.__tamatsuzuraStorage = new TamaTsuzuraDraftStorageBridge();
globalThis.__tamatsuzuraFiles = new TamaTsuzuraFileTransferBridge();
