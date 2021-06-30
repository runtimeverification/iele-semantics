ARG BASE_IMAGE=ubuntu:bionic
FROM ${BASE_IMAGE}
ARG BASE_IMAGE

ENV TZ=America/Chicago
RUN    ln --symbolic --no-dereference --force /usr/share/zoneinfo/$TZ /etc/localtime \
    && echo $TZ > /etc/timezone

RUN    apt-get update         \
    && apt-get upgrade --yes  \
    && apt-get install --yes  \
            clang-8           \
            cmake             \
            curl              \
            dh-make           \
            git               \
            jq                \
            libcrypto++-dev   \
            libffi-dev        \
            libgflags-dev     \
            libjemalloc-dev   \
            libprocps-dev     \
            libprotobuf-dev   \
            libsecp256k1-dev  \
            libssl-dev        \
            netcat-openbsd    \
            opam              \
            pkg-config        \
            protobuf-compiler \
            python3-pip       \
            rapidjson-dev     \
            sudo

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN pip3 install dataclasses dacite

ARG USER_ID=1000
ARG GROUP_ID=1000
RUN    groupadd -g $GROUP_ID user                                                \
    && useradd --create-home --uid $USER_ID --shell /bin/sh --gid $GROUP_ID user \
    && echo "user ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
