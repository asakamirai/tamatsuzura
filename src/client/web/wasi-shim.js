// Minimal zero-dependency WASI preview1 shim for GHC 9.12 Wasm reactor modules in browsers.
// Avoids external CDN dependencies so TamaTsuzura works cleanly in self-hosted CGI and offline PWA deployments.

const ERRNO_SUCCESS = 0;
const ERRNO_BADF = 8;
const ERRNO_INVAL = 28;
const ERRNO_NOSYS = 52;

const FILETYPE_CHARACTER_DEVICE = 2;

export class BrowserWasiShim {
  constructor({ args = [], env = [], onStdout = null, onStderr = null } = {}) {
    this._encoder = new TextEncoder();
    this._decoders = {
      1: new TextDecoder("utf-8", { fatal: false }),
      2: new TextDecoder("utf-8", { fatal: false }),
    };
    this._args = args.map((s) => this._encoder.encode(`${s}\0`));
    this._env = env.map((s) => this._encoder.encode(`${s}\0`));
    this._onStdout = onStdout || ((line) => console.log(`[tamatsuzura] ${line}`));
    this._onStderr = onStderr || ((line) => console.error(`[tamatsuzura] ${line}`));
    this._buffers = { 1: "", 2: "" };
    this._memory = null;
    this.wasiImport = this._buildImports();
  }

  initialize(instance) {
    this._memory = instance.exports.memory;
    if (typeof instance.exports._initialize === "function") {
      instance.exports._initialize();
    }
  }

  _view() {
    if (!this._memory) {
      throw new Error("WASI shim memory is not initialized");
    }
    return new DataView(this._memory.buffer);
  }

  _bytes() {
    if (!this._memory) {
      throw new Error("WASI shim memory is not initialized");
    }
    return new Uint8Array(this._memory.buffer);
  }

  _flushText(fd, chunk) {
    const combined = (this._buffers[fd] || "") + chunk;
    const lines = combined.split("\n");
    this._buffers[fd] = lines.pop() || "";
    const sink = fd === 1 ? this._onStdout : this._onStderr;
    for (const line of lines) {
      sink(line);
    }
  }

  _writeStringTable(entries, ptrTablePtr, bufPtr) {
    const view = this._view();
    const bytes = this._bytes();
    let offset = bufPtr;
    for (let i = 0; i < entries.length; i += 1) {
      view.setUint32(ptrTablePtr + i * 4, offset, true);
      bytes.set(entries[i], offset);
      offset += entries[i].length;
    }
    return ERRNO_SUCCESS;
  }

  _buildImports() {
    const self = this;

    return {
      args_sizes_get(argcPtr, argvBufSizePtr) {
        const view = self._view();
        const totalBytes = self._args.reduce((acc, item) => acc + item.length, 0);
        view.setUint32(argcPtr, self._args.length, true);
        view.setUint32(argvBufSizePtr, totalBytes, true);
        return ERRNO_SUCCESS;
      },

      args_get(argvPtr, argvBufPtr) {
        return self._writeStringTable(self._args, argvPtr, argvBufPtr);
      },

      environ_sizes_get(countPtr, bufSizePtr) {
        const view = self._view();
        const totalBytes = self._env.reduce((acc, item) => acc + item.length, 0);
        view.setUint32(countPtr, self._env.length, true);
        view.setUint32(bufSizePtr, totalBytes, true);
        return ERRNO_SUCCESS;
      },

      environ_get(environPtr, environBufPtr) {
        return self._writeStringTable(self._env, environPtr, environBufPtr);
      },

      clock_res_get(_clockId, resolutionPtr) {
        const view = self._view();
        view.setBigUint64(resolutionPtr, 1_000_000n, true);
        return ERRNO_SUCCESS;
      },

      clock_time_get(clockId, _precision, timePtr) {
        const view = self._view();
        let ns;
        if (clockId === 0 || typeof performance === "undefined") {
          // CLOCK_REALTIME (or fallback when performance.now() is unavailable)
          ns = BigInt(Date.now()) * 1_000_000n;
        } else {
          // CLOCK_MONOTONIC or process/thread clocks
          ns = BigInt(Math.floor(performance.now() * 1_000_000));
        }
        view.setBigUint64(timePtr, ns, true);
        return ERRNO_SUCCESS;
      },

      fd_close(fd) {
        if (fd <= 2) {
          return ERRNO_SUCCESS;
        }
        return ERRNO_BADF;
      },

      fd_fdstat_get(fd, statPtr) {
        if (fd < 0 || fd > 2) {
          return ERRNO_BADF;
        }
        self._bytes().fill(0, statPtr, statPtr + 24);
        const view = self._view();
        view.setUint8(statPtr, FILETYPE_CHARACTER_DEVICE);
        view.setUint16(statPtr + 2, 0, true);
        view.setBigUint64(statPtr + 8, 0xffffffffffffffffn, true);
        view.setBigUint64(statPtr + 16, 0xffffffffffffffffn, true);
        return ERRNO_SUCCESS;
      },

      fd_fdstat_set_flags(_fd, _flags) {
        return ERRNO_SUCCESS;
      },

      fd_filestat_get(fd, statPtr) {
        if (fd < 0 || fd > 2) {
          return ERRNO_BADF;
        }
        const bytes = self._bytes();
        bytes.fill(0, statPtr, statPtr + 64);
        self._view().setUint8(statPtr + 16, FILETYPE_CHARACTER_DEVICE);
        return ERRNO_SUCCESS;
      },

      fd_filestat_set_size(_fd, _size) {
        return ERRNO_BADF;
      },

      fd_filestat_set_times(_fd, _atim, _mtim, _fstFlags) {
        return ERRNO_BADF;
      },

      fd_advise(_fd, _offset, _len, _advice) {
        return ERRNO_SUCCESS;
      },

      fd_allocate(_fd, _offset, _len) {
        return ERRNO_BADF;
      },

      fd_fdstat_set_rights(_fd, _fsRightsBase, _fsRightsInheriting) {
        return ERRNO_SUCCESS;
      },

      fd_prestat_get(_fd, _prestatPtr) {
        return ERRNO_BADF;
      },

      fd_prestat_dir_name(_fd, _pathPtr, _pathLen) {
        return ERRNO_BADF;
      },

      fd_read(fd, _iovsPtr, _iovsLen, nreadPtr) {
        if (fd !== 0) {
          return ERRNO_BADF;
        }
        self._view().setUint32(nreadPtr, 0, true);
        return ERRNO_SUCCESS;
      },

      fd_pread(_fd, _iovsPtr, _iovsLen, _offset, _nreadPtr) {
        return ERRNO_BADF;
      },

      fd_write(fd, iovsPtr, iovsLen, nwrittenPtr) {
        if (fd !== 1 && fd !== 2) {
          return ERRNO_BADF;
        }
        const view = self._view();
        const bytes = self._bytes();
        const decoder = self._decoders[fd];
        let written = 0;
        for (let i = 0; i < iovsLen; i += 1) {
          const ptr = view.getUint32(iovsPtr + i * 8, true);
          const len = view.getUint32(iovsPtr + i * 8 + 4, true);
          if (len > 0) {
            const chunk = decoder.decode(bytes.subarray(ptr, ptr + len), { stream: true });
            self._flushText(fd, chunk);
            written += len;
          }
        }
        view.setUint32(nwrittenPtr, written, true);
        return ERRNO_SUCCESS;
      },

      fd_pwrite(_fd, _iovsPtr, _iovsLen, _offset, _nwrittenPtr) {
        return ERRNO_BADF;
      },

      fd_seek(_fd, _offset, _whence, _newOffsetPtr) {
        return ERRNO_INVAL;
      },

      fd_tell(_fd, _offsetPtr) {
        return ERRNO_INVAL;
      },

      fd_sync(_fd) {
        return ERRNO_SUCCESS;
      },

      fd_datasync(_fd) {
        return ERRNO_SUCCESS;
      },

      fd_readdir(_fd, _bufPtr, _bufLen, _cookie, _bufUsedPtr) {
        return ERRNO_BADF;
      },

      fd_renumber(_fromFd, _toFd) {
        return ERRNO_NOSYS;
      },

      path_open() {
        return ERRNO_BADF;
      },

      path_filestat_get() {
        return ERRNO_BADF;
      },

      path_create_directory() {
        return ERRNO_BADF;
      },

      path_remove_directory() {
        return ERRNO_BADF;
      },

      path_unlink_file() {
        return ERRNO_BADF;
      },

      path_rename() {
        return ERRNO_BADF;
      },

      path_readlink() {
        return ERRNO_BADF;
      },

      poll_oneoff(_inPtr, _outPtr, _nsubscriptions, neventsPtr) {
        self._view().setUint32(neventsPtr, 0, true);
        return ERRNO_NOSYS;
      },

      proc_exit(code) {
        throw new Error(`WASI proc_exit(${code})`);
      },

      random_get(bufPtr, bufLen) {
        const bytes = self._bytes().subarray(bufPtr, bufPtr + bufLen);
        if (typeof crypto !== "undefined" && typeof crypto.getRandomValues === "function") {
          // crypto.getRandomValues has a 65536-byte limit per call
          const maxChunk = 65536;
          for (let offset = 0; offset < bytes.length; offset += maxChunk) {
            crypto.getRandomValues(bytes.subarray(offset, Math.min(offset + maxChunk, bytes.length)));
          }
          return ERRNO_SUCCESS;
        }
        for (let i = 0; i < bytes.length; i += 1) {
          bytes[i] = Math.floor(Math.random() * 256);
        }
        return ERRNO_SUCCESS;
      },

      sched_yield() {
        return ERRNO_SUCCESS;
      },
    };
  }
}
