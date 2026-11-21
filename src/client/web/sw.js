// TamaTsuzura Service Worker — App Shell & Wasm asset cache for offline launch.
// The build script injects the deterministic content hash and precache asset list
// so every release triggers a clean Service Worker update and purges stale caches.

const CACHE_PREFIX = "tamatsuzura-shell-";
const CACHE_VERSION = "__TAMATSUZURA_CACHE_VERSION__";
const CACHE_NAME = `${CACHE_PREFIX}${CACHE_VERSION}`;

const APP_SHELL_ASSETS = __TAMATSUZURA_APP_SHELL_ASSETS__;

function isApiPathname(pathname) {
  return (
    pathname === "/api" ||
    pathname.startsWith("/api/") ||
    pathname.includes("/api/") ||
    pathname.endsWith("/api")
  );
}

function isServiceWorkerScript(url) {
  return url.pathname.endsWith("/sw.js");
}

function isCacheableShellResponse(response) {
  return Boolean(response && response.ok && response.type === "basic");
}

function isHtmlResponse(response) {
  if (!isCacheableShellResponse(response)) {
    return false;
  }
  const contentType = (response.headers.get("content-type") || "").toLowerCase();
  return contentType.includes("text/html");
}

async function precacheAppShell(cache) {
  await Promise.all(
    APP_SHELL_ASSETS.map(async (asset) => {
      // Bypass heuristic HTTP caches during SW install so versioned caches never mix old/new Wasm+JS.
      const response = await fetch(asset, { cache: "no-cache" });
      if (!isCacheableShellResponse(response)) {
        throw new Error(`Failed to precache ${asset}: HTTP ${response.status}`);
      }
      await cache.put(asset, response);
    })
  );
}

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches
      .open(CACHE_NAME)
      .then((cache) => precacheAppShell(cache))
      .then(() => self.skipWaiting())
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(
          keys
            .filter((key) => key.startsWith(CACHE_PREFIX) && key !== CACHE_NAME)
            .map((key) => caches.delete(key))
        )
      )
      .then(() => self.clients.claim())
  );
});

self.addEventListener("message", (event) => {
  if (event.data && event.data.type === "SKIP_WAITING") {
    self.skipWaiting();
  }
});

self.addEventListener("fetch", (event) => {
  const { request } = event;
  if (request.method !== "GET") {
    return;
  }

  const url = new URL(request.url);
  if (url.origin !== self.location.origin) {
    return;
  }

  // Never intercept or cache REST API calls or the Service Worker script itself.
  if (isApiPathname(url.pathname) || isServiceWorkerScript(url)) {
    return;
  }

  // Navigation requests: Network-First so online users receive updated HTML,
  // falling back to the precached App Shell when offline.
  if (request.mode === "navigate") {
    event.respondWith(
      fetch(request)
        .then((response) => {
          if (isHtmlResponse(response)) {
            const copy = response.clone();
            caches.open(CACHE_NAME).then((cache) => cache.put("./index.html", copy)).catch(() => {});
          }
          return response;
        })
        .catch(() =>
          caches.open(CACHE_NAME).then((cache) =>
            cache.match("./index.html").then((cached) => cached || cache.match("./"))
          )
        )
    );
    return;
  }

  // Static app-shell assets (Wasm/JS/CSS/icons/manifest): Cache-First for fast
  // mobile startup and offline availability, backed by versioned cache names.
  event.respondWith(
    caches.open(CACHE_NAME).then((cache) =>
      cache.match(request).then((cached) => {
        if (cached) {
          return cached;
        }
        return fetch(request).then((response) => {
          if (isCacheableShellResponse(response)) {
            cache.put(request, response.clone()).catch(() => {});
          }
          return response;
        });
      })
    )
  );
});
