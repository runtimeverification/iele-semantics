ARG K_COMMIT
ARG DISTRO
FROM runtimeverificationinc/kframework-k:ubuntu-${DISTRO}-${K_COMMIT}

COPY kiele_amd64_${DISTRO}.deb /kiele_amd64_${DISTRO}.deb
RUN    apt-get update                                   \
    && apt-get upgrade --yes                            \
    && apt-get install --yes /kiele_amd64_${DISTRO}.deb \
    && rm -rf /kiele_amd64_${DISTRO}.deb
