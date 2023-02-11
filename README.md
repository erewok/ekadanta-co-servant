# ekadanta-co

This site has been built with the following tools and technologies:

- Haskell
- [Servant](https://github.com/haskell-servant/servant)
- Elasticsearch
- Docker

## Run Locally with GHC

If you have a Haskell compiler, run:

```sh
$ cabal build

```

## Run Locally with docker-compose

If you have Docker installed, you can build the Docker image and run this project like this:

```sh
$ docker build -t ekadanta-co:latest
...
$ export TAG=latest

$ docker-compose up

```