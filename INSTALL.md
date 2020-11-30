Installing KIELE
================

We provide packages for IELE on the following platforms:

-   Ubuntu Bionic (18.04)
-   From Source Build

**NOTE**: We do not currently support running K on native Windows.
To use K on Windows, you are encouraged to install [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and follow the instructions for Ubuntu Bionic.

Download Packages
-----------------

Download the appropriate package from the GitHub, via the [Releases](https://github.com/kframework/k/releases) page.
Releases are generated as often as possible from `master` build.

Install Packages
----------------

For version `X.Y.Z`, the following instructions tell you how to install the downloaded package on your system.

### Ubuntu Bionic (18.04)

First make sure you have the `kframework` package installed, see instructions here: <https://github.com/kframework/k/releases>.
Then install the `kiele` package.

```sh
sudo apt install ./kiele_X.Y.Z_amd64_bionic.deb
```

From Source Build
-----------------

### System Dependencies

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

### Build K and KIELE

These commands build and install K and KIELE:

```sh
git clone git@github.com:runtimeverification/iele-semantics.git
cd iele-semantics
git submodule update --init --recursive
curl -sSL https://github.com/kframework/k/releases/download/$(cat deps/k_release)/kframework_5.0.0_amd64_bionic.deb
sudo apt-get install --yes ./kframework_5.0.0_amd64_bionic.deb
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot k-configure-opam'
make COVERAGE=k
```
