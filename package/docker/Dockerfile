ARG BASE_IMAGE=ubuntu:bionic
FROM ${BASE_IMAGE}
ARG BASE_IMAGE

ENV TZ=America/Chicago
RUN    ln --symbolic --no-dereference --force /usr/share/zoneinfo/$TZ /etc/localtime \
    && echo $TZ > /etc/timezone

RUN    apt-get update           \
    && apt-get upgrade --yes    \
    && apt-get install --yes    \
                build-essential

ARG KIELE_VERSION
COPY kiele_${KIELE_VERSION}_amd64_bionic.deb /kiele_${KIELE_VERSION}_amd64_bionic.deb
RUN    apt-get update        \
    && apt-get upgrade --yes \
    && apt-get install --yes /kiele_${KIELE_VERSION}_amd64_bionic.deb
RUN rm -rf /kiele_${KIELE_VERSION}_amd64_bionic.deb
