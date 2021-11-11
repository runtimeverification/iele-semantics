Installing KIELE
================

We support the following installation methods:

-   [Ubuntu package](#ubuntu-package)
-   [Linux binary package](#linux-binary-package) (most other distributions)
-   [Nix](#nix) (Linux and macOS)
-   [Docker Images](#docker-images)
-   [Build from source](#build-from-source) (Linux and macOS)

Our most up-to-date packages are available at <https://github.com/runtimeverification/iele-semantics/releases>.

**NOTE**: We do not currently support running K on native Windows.
To use K on Windows, you are encouraged to install [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and follow the instructions for Ubuntu Bionic.

## Ubuntu Package

We provide both an Ubuntu Bionic (18.04) and Ubuntu Focal (20.04) package.

First make sure you have the appropriate `kframework` package installed, see instructions here: <https://github.com/kframework/k/releases>.

Download the appropriate Ubuntu package from the GitHub, via the [Releases](https://github.com/runtimeverification/iele-semantics/releases) page.
Releases are generated as often as possible from `master` build.
Assuming you have downloaded KIELE version `X.Y.Z` for distro `DISTRO`, install the package with `apt`:

```sh
sudo apt install ./kiele_X.Y.Z_amd64_DISTRO.deb
```

## Linux binary package

Install the following runtime dependencies:

```sh
sudo apt-get install --yes libcrypto++-dev libjemalloc-dev libmpfr-dev libprotobuf-dev libsecp256k1-dev
```

Download the "KIELE Linux Binary" package from the GitHub, via the [Releases](https://github.com/runtimeverification/iele-semantics/releases) page.
Releases are generated as often as possible from `master` build.

Assuming you have downloaded KIELE version `X.Y.Z`, extract the tarball:

```sh
tar -xvf kiele-X.Y.Z-bin.tar.gz
```

Copy all the files in the tarball into place:

```sh
mkdir -p ~/.local/bin ~/.local/lib
cp -r kiele-X.Y.Z-bin/usr/bin/* ~/.local/bin/
cp -r kiele-X.Y.Z-bin/usr/lib/* ~/.local/lib/
```

And make sure it's on `PATH`:

```sh
export PATH=$HOME/.local/bin:$PATH
```

## Nix

A Nix expression is provided, with binary caching for Linux and macOS.

### Prerequisites

It is safe to skip any of these dependencies that are already installed.

Follow the instructions below.
Perform all steps as your normal (non-root) user.
You may find the same instructions and our public key at <https://runtimeverification.cachix.org>.
To follow this instructions, you will need `curl` installed on your system <https://curl.haxx.se/download.html>.

#### Nix

Please pay careful attention to any additional instructions printed by the installer.
Install Nix:

```.sh
# Linux and macOS < 10.15
bash <(curl -L https://nixos.org/nix/install)

# macOS 10.15 (See note below)
bash <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
```

For macOS 10.15, please refer to the [Nix manual](https://nixos.org/manual/nix/stable/#sect-macos-installation) for more information.

#### Cachix

Install Cachix and start using the binary cache:

```.sh
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use runtimeverification
```

### Install with Nix

With the [prerequisites](#prerequisites) installed,
we can install `kiele` from any clone of the repository:

```.sh
git clone https://github.com/runtimeverification/iele-semantics
cd iele-semantics
git submodule update --init --recursive
nix-env -f . -i kiele
```

## Docker Images

Docker images with KIELE pre-installed are available at the
[runtimeverificationinc/runtimeverification-iele-semantics Docker Hub repository](https://hub.docker.com/repository/docker/runtimeverificationinc/runtimeverification-iele-semantics).

Each release at `COMMIT_ID` for Ubuntu platform DISTRO has an image associated with it at
`runtimeverificationinc/runtimeverification-iele-semantics:ubuntu-DISTRO-COMMIT_ID`.
The latest `master` build Docker image can be accessed with `COMMIT_ID` set to
`master`.

To run the image directly:

```sh
docker run -it runtimeverificationinc/runtimeverification-iele-semantics:ubuntu-DISTRO-COMMIT_ID
```

and to make a Docker Image based on it, use the following line in your
`Dockerfile`:

```Dockerfile
FROM runtimeverificationinc/runtimeverification-iele-semantics:ubuntu-DISTRO-COMMIT_ID
```
# Build from source

## Installing K

First make sure you have the `kframework` package installed.

Look in `deps/k_release` for the currently supported tag release of K, you will need to install that one.

see instructions here: <https://github.com/kframework/k/releases>.
Releases are available for download here: <https://github.com/kframework/k/tags>.

## System Dependencies

### Z3

KIELE requires Z3 version 4.8.11, which you may need to install from a source
build if your package manager supplies a different version. To do so, follow the
instructions
[here](https://github.com/Z3Prover/z3#building-z3-using-make-and-gccclang) after
checking out the correct tag in the Z3 repository:

```sh
git clone https://github.com/Z3Prover/z3.git
cd z3
git checkout z3-4.8.11
python scripts/mk_make.py
cd build
make
sudo make install
```

### Ubuntu Bionic

The following packages are needed for running KIELE on Ubuntu:

```sh
sudo apt-get install --yes autoconf build-essential cmake curl flex gcc   \
                           libcrypto++-dev libffi-dev libmpfr-dev         \
                           libprocps-dev libprotobuf-dev libsecp256k1-dev \
                           libssl-dev libtool make maven netcat opam      \
                           openjdk-8-jdk pkg-config protobuf-compiler     \
                           python3 zlib1g-dev
```

### MacOS

```sh
brew tap adoptopenjdk/openjdk
brew install --cask adoptopenjdk8
brew install maven opam pkg-config gmp mpfr automake libtool protobuf cmake openssl
```

On all systems, you need Haskell Stack:

```sh
curl -sSL https://get.haskellstack.org/ | sh
```

## Build KIELE

These commands build and install KIELE:

```sh
git clone https://github.com/runtimeverification/iele-semantics.git
cd iele-semantics
git submodule update --init --recursive
opam init
opam install --yes ocaml-protoc rlp yojson zarith hex uuidm cryptokit
eval $(opam config env)
make build -j4

sudo make install
```
