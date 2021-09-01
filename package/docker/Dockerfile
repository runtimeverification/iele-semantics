ARG K_COMMIT
ARG DISTRO
FROM runtimeverificationinc/kframework-k:ubuntu-${DISTRO}-${K_COMMIT}

RUN    apt-get update        \
    && apt-get upgrade       \
    && apt-get install --yes \
                autoconf     \
                libgmp-dev   \
                pkg-config   \
                zlib1g-dev

ARG DISTRO
COPY kiele_amd64_${DISTRO}.deb /kiele_amd64_${DISTRO}.deb
RUN    apt-get update                                   \
    && apt-get upgrade --yes                            \
    && apt-get install --yes /kiele_amd64_${DISTRO}.deb \
    && rm -rf /kiele_amd64_${DISTRO}.deb

RUN    if [ "${DISTRO}" = "bionic" ]; then opam init --yes; else opam init --yes --disable-sandboxing; fi \
    && opam install --yes zarith hex uuidm rlp yojson cryptokit ocaml-protoc
