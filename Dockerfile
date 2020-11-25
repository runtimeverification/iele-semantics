ARG K_COMMIT
FROM runtimeverificationinc/kframework-k:ubuntu-bionic-${K_COMMIT}

RUN    apt update         \
    && apt upgrade --yes  \
    && apt install --yes  \
        autoconf          \
        build-essential   \
        cmake             \
        curl              \
        flex              \
        gcc               \
        libcrypto++-dev   \
        libffi-dev        \
        libmpfr-dev       \
        libprocps-dev     \
        libprotobuf-dev   \
        libsecp256k1-dev  \
        libssl-dev        \
        libtool           \
        make              \
        maven             \
        netcat            \
        opam              \
        openjdk-8-jdk     \
        pandoc            \
        pkg-config        \
        protobuf-compiler \
        python3           \
        zlib1g-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN    apt-get update               \
    && apt-get upgrade --yes        \
    && apt-get install --yes nodejs

ARG USER_ID=1000
ARG GROUP_ID=1000
RUN    groupadd --gid $GROUP_ID user                                        \
    && useradd --create-home --uid $USER_ID --shell /bin/sh --gid user user

USER $USER_ID:$GROUP_ID

ENV LC_ALL=C.UTF-8
ADD --chown=user:user iele-assemble/stack.yaml iele-assemble/iele-assemble.cabal /home/user/.tmp-haskell/
RUN    cd /home/user/.tmp-haskell \
    && stack build --only-snapshot

RUN    opam init -y \
    && opam install zarith hex uuidm rlp yojson cryptokit ocaml-protoc

RUN    git config --global user.email 'admin@runtimeverification.com' \
    && git config --global user.name  'RV Jenkins'                    \
    && mkdir -p ~/.ssh                                                \
    && echo 'host github.com'                       > ~/.ssh/config   \
    && echo '    hostname github.com'              >> ~/.ssh/config   \
    && echo '    user git'                         >> ~/.ssh/config   \
    && echo '    identityagent SSH_AUTH_SOCK'      >> ~/.ssh/config   \
    && echo '    stricthostkeychecking accept-new' >> ~/.ssh/config   \
    && chmod go-rwx -R ~/.ssh
