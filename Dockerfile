ARG K_COMMIT
FROM runtimeverificationinc/kframework-k:ubuntu-bionic-${K_COMMIT}

RUN    apt update                                                          \
    && apt upgrade --yes                                                   \
    && apt install --yes                                                   \
           autoconf build-essential curl flex gcc libffi-dev libmpfr-dev   \
           libtool make maven opam openjdk-8-jdk pandoc pkg-config python3 \
           zlib1g-dev libsecp256k1-dev netcat protobuf-compiler            \
           libprotobuf-dev libcrypto++-dev libssl-dev libprocps-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

ARG USER_ID=1000
ARG GROUP_ID=1000
RUN    groupadd --gid $GROUP_ID user                                        \
    && useradd --create-home --uid $USER_ID --shell /bin/sh --gid user user

USER $USER_ID:$GROUP_ID

ENV LC_ALL=C.UTF-8
ADD --chown=user:user compiler/stack.yaml /home/user/.tmp-haskell/
RUN    cd /home/user/.tmp-haskell \
-    && stack build --only-snapshot
