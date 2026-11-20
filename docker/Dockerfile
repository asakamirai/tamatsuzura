FROM ubuntu:22.04

ARG HLINT_VERSION=3.8

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Install base dependencies (including prerequisites for Stack, Haskell builds, and ghc-wasm-meta)
RUN apt-get update && apt-get install -y --no-install-recommends \
    autoconf \
    automake \
    build-essential \
    bzip2 \
    ca-certificates \
    curl \
    git \
    gnupg \
    jq \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libsqlite3-dev \
    netbase \
    patch \
    pkg-config \
    python3 \
    unzip \
    xz-utils \
    zlib1g-dev \
    zstd \
    && rm -rf /var/lib/apt/lists/*

# Install Haskell Stack (refresh apt index so the installer's apt-get checks succeed)
RUN apt-get update \
    && curl -fsSL https://get.haskellstack.org/ | sh \
    && rm -rf /var/lib/apt/lists/*

# Install HLint:
# - On x86_64, keep the extracted directory (including data/hlint.yaml) under /opt/hlint
#   and expose a wrapper script so data files are always resolved.
# - On non-x86_64, fall back to Ubuntu's hlint package (note: Ubuntu 22.04 ships an older
#   hlint 3.3.x that may not parse newer GHC 9.x extensions such as OverloadedRecordDot).
RUN ARCH="$(uname -m)" \
    && if [ "${ARCH}" = "x86_64" ]; then \
         mkdir -p /opt/hlint \
         && curl -fsSL "https://github.com/ndmitchell/hlint/releases/download/v${HLINT_VERSION}/hlint-${HLINT_VERSION}-x86_64-linux.tar.gz" \
           | tar -xz --strip-components=1 -C /opt/hlint \
         && printf '#!/bin/sh\nexport hlint_datadir=/opt/hlint/data\nexec /opt/hlint/hlint "$@"\n' > /usr/local/bin/hlint \
         && chmod +x /opt/hlint/hlint /usr/local/bin/hlint; \
       else \
         apt-get update && apt-get install -y --no-install-recommends hlint \
         && rm -rf /var/lib/apt/lists/*; \
       fi \
    && hlint --version \
    && printf 'module Main where\nmain :: IO ()\nmain = putStrLn "ok"\n' | hlint -

# Install GHC WebAssembly backend (ghc-wasm-meta; GHC 9.12 required by Miso 1.9+ Wasm JSFFI / MultilineStrings)
ARG GHC_WASM_FLAVOUR=9.12
RUN export FLAVOUR="${GHC_WASM_FLAVOUR}" \
    && curl --proto '=https' --tlsv1.2 -fsSL https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | sh \
    && printf '#!/bin/sh\nset -eu\n. /root/.ghc-wasm/env\nexec "$@"\n' > /usr/local/bin/ghc-wasm-exec \
    && chmod +x /usr/local/bin/ghc-wasm-exec \
    && ghc-wasm-exec wasm32-wasi-ghc --version \
    && ghc-wasm-exec wasm32-wasi-cabal --version \
    && ghc-wasm-exec wasm32-wasi-cabal update

# Install Node.js (LTS) for client/wasm/tooling
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && npm install -g pnpm \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

CMD ["/bin/bash"]
