FROM ubuntu:22.04

ARG HLINT_VERSION=3.8

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Install base dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    git \
    libgmp-dev \
    libncurses-dev \
    libsqlite3-dev \
    pkg-config \
    zlib1g-dev \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install Haskell Stack
RUN curl -fsSL https://get.haskellstack.org/ | sh

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

# Install Node.js (LTS) for client/wasm/tooling
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && npm install -g pnpm \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

CMD ["/bin/bash"]
