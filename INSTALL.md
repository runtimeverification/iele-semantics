Installing IELE
===============

These instructions are an example for how to setup IELE on an Ubuntu 18.04 machine (and MacOS).

System Dependencies
-------------------

The following packages are needed for running KIELE on Ubuntu:

```sh
sudo apt-get install --yes autoconf build-essential cmake curl flex gcc   \
                           libcrypto++-dev libffi-dev libmpfr-dev         \
                           libprocps-dev libprotobuf-dev libsecp256k1-dev \
                           libssl-dev libtool make maven netcat opam      \
                           openjdk-8-jdk pkg-config protobuf-compiler     \
                           python3 zlib1g-dev
```

On MacOS, you need the following:

```sh
brew tap homebrew/homebrew-cask homebrew-cask-versions
brew cask install java8
brew install maven opam pkg-config gmp mpfr automake libtool
```

On all systems, you need Haskell Stack:

```sh
curl -sSL https://get.haskellstack.org/ | sh
```

Build K and KIELE
-----------------

These commands build and install K and KIELE:

```sh
git clone git@github.com:runtimeverification/iele-semantics.git
cd iele-semantics
git submodule update --init --recursive
curl -sSL https://github.com/kframework/k/releases/tags/$(cat deps/k_release)/kframework_5.0.0_amd64_bionic.deb
sudo apt-get install --yes ./kframework_5.0.0_amd64_bionic.deb
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot k-configure-opam'
make COVERAGE=k
```
