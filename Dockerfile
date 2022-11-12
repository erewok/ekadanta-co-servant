FROM debian:stretch-slim as builder

ENV LANG C.UTF-8
SHELL ["/bin/bash", "-Eeuxo", "pipefail", "-c"]

RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    build-essential \
    libffi-dev \
    libgmp-dev \
    zlib1g-dev \
    gnupg2 \
    dirmngr \
    curl \
    ca-certificates \
    netbase \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /opt/ekadanta-co/bin

RUN echo 'deb http://downloads.haskell.org/debian stretch main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA3CBA3FFE22B574 && \
    apt-get update && \
    apt-get install -y --no-install-recommends ghc-9.2.4 cabal-install-3.6

ENV PATH /root/.cabal/bin:/root/.local/bin:/opt/cabal/3.6/bin:/opt/ghc/9.2.4/bin:$PATH

WORKDIR /opt/ekadanta-co/

COPY ekadanta-co.cabal ChangeLog.md README.md /opt/ekadanta-co/

RUN cabal update && cabal build --only-dependencies

COPY . /opt/ekadanta-co

RUN cabal install

FROM debian:stretch-slim as base_os

RUN apt-get update && \
    apt-get install -y \
    build-essential \
    libffi-dev \
    libgmp-dev \
    curl \
    ca-certificates \
    netbase

COPY --from=builder /root/.cabal/bin/ekadanta-co /opt/ekadanta-co/bin/

WORKDIR /opt/ekadanta-co/

COPY static ./static

RUN adduser --disabled-password --gecos "" ekadanta \
    && chown -R ekadanta:ekadanta /opt/ekadanta-co

USER ekadanta
EXPOSE 8000

CMD ["/opt/ekadanta-co/bin/ekadanta-co"]
