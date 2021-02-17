ARG K_COMMIT
FROM runtimeverificationinc/kframework-k:ubuntu-bionic-${K_COMMIT}

COPY kiele_amd64_bionic.deb /kiele_amd64_bionic.deb
RUN    apt-get update                                \
    && apt-get upgrade --yes                         \
    && apt-get install --yes /kiele_amd64_bionic.deb \
    && rm -rf /kiele_amd64_bionic.deb
