import "./editor-bridge.js";
import { BrowserWasiShim } from "./wasi-shim.js";
import ghcWasmJsffi from "./ghc_wasm_jsffi.js";

function renderBootError(error) {
  const detail = error && error.message ? error.message : String(error);
  console.error("[tamatsuzura] Failed to boot Wasm client:", error);

  const root = document.getElementById("tt-boot-status") || document.body;
  root.textContent = "";

  const box = document.createElement("div");
  box.className = "tt-boot-screen tt-boot-error";

  const heading = document.createElement("h1");
  heading.textContent = "TamaTsuzura の起動に失敗しました";

  const body = document.createElement("p");
  body.textContent = `WebAssembly モジュールの読み込み中にエラーが発生しました: ${detail}`;

  box.appendChild(heading);
  box.appendChild(body);
  root.appendChild(box);
}

async function instantiateWasm(url, imports) {
  const response = await fetch(url, { credentials: "same-origin" });
  if (!response.ok) {
    throw new Error(`HTTP ${response.status} while fetching ${url}`);
  }
  if (typeof WebAssembly.instantiateStreaming === "function") {
    try {
      return await WebAssembly.instantiateStreaming(response.clone(), imports);
    } catch (_streamingErr) {
      // Fallback for servers that do not send application/wasm Content-Type
    }
  }
  const bytes = await response.arrayBuffer();
  return WebAssembly.instantiate(bytes, imports);
}

async function boot() {
  const wasi = new BrowserWasiShim({
    args: ["tamatsuzura-client"],
    env: [],
  });

  const wasmExports = {};
  const importObject = {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghcWasmJsffi(wasmExports),
  };

  const { instance } = await instantiateWasm("./tamatsuzura-client.wasm", importObject);
  Object.assign(wasmExports, instance.exports);
  wasi.initialize(instance);

  // Miso startApp draws onto an empty <body>
  document.body.textContent = "";

  if (typeof instance.exports.hs_start !== "function") {
    throw new Error("Missing exported hs_start entry point in tamatsuzura-client.wasm");
  }
  await instance.exports.hs_start();
}

boot().catch(renderBootError);
