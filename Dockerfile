FROM haskell:9.2.5-slim-buster as builder

ENV LANG C.UTF-8

ENV PATH=/root/.cabal/bin:/root/.local/bin:/opt/cabal/3.8/bin:/opt/ghc/9.2.5/bin:$PATH \
    APP_DIR=/opt/ekadanta-co/

RUN mkdir -p "${APP_DIR}/bin"
WORKDIR ${APP_DIR}
# Copy build-required files
COPY ekadanta-co.cabal README.md ${APP_DIR}/
# Copy source code
COPY . ${APP_DIR}

# Build sys deps, build project deps, remove all afterward
RUN savedAptMark="$(apt-mark showmanual)"; \
    set -eEx; \
    apt-get update; \
    apt-get install --no-install-recommends -y \
    build-essential \
    libffi-dev \
    libgmp-dev \
    zlib1g-dev \
    gnupg2 \
    dirmngr \
    curl \
    ca-certificates \
    netbase; \
    cabal update && cabal build --only-dependencies; \
    cabal install; \
    apt-mark auto '.*' > /dev/null; \
    apt-mark manual $savedAptMark; \
    apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false; \
    rm -rf /var/lib/apt/lists/*

FROM debian:bullseye-slim as base_os

COPY --from=builder /root/.cabal/bin/ekadanta-co /opt/ekadanta-co/bin/

WORKDIR /opt/ekadanta-co/

COPY static ./static

RUN adduser --disabled-password --gecos "" ekadanta \
    && chown -R ekadanta:ekadanta /opt/ekadanta-co

USER ekadanta
EXPOSE 8000

CMD ["/opt/ekadanta-co/bin/ekadanta-co"]
