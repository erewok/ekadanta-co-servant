FROM debian:stretch-slim as builder

SHELL ["/bin/bash", "-Eeuxo", "pipefail", "-c"]

RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    build-essential \
    libffi-dev \
    libgmp-dev \
    zlib1g-dev \
    curl \
    ca-certificates \
    tcl \
    netbase \
    && curl -sSL "https://get.haskellstack.org/" | sh \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /opt/ekadanta-co/bin

WORKDIR /opt/ekadanta-co/

COPY stack.yaml package.yaml ChangeLog.md README.md /opt/ekadanta-co/

RUN stack --no-terminal --install-ghc build --only-dependencies

COPY . /opt/ekadanta-co

RUN stack install

FROM debian:stretch-slim as base_os

RUN apt-get update && \
    apt-get install -y \
    build-essential \
    libffi-dev \
    libgmp-dev \
    curl \
    ca-certificates \
    netbase

COPY --from=builder /root/.local/bin/ekadanta-co /opt/ekadanta-co/bin/

RUN adduser --disabled-password --gecos "" ekadanta \
    && chown -R ekadanta:ekadanta /opt/ekadanta-co

USER ekadanta
EXPOSE 8000

CMD ["/opt/ekadanta-co/bin"]
