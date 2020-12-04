Installing KIELE
================

We support the following installation methods:

-   [Ubuntu Bionic (18.04) package](#ubuntu-bionic-package)
-   [Linux binary package](#linux-binary-package) (most other distributions)
-   [Build from source](#build-from-source) (Linux and macOS)

**NOTE**: We do not currently support running K on native Windows.
To use K on Windows, you are encouraged to install [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and follow the instructions for Ubuntu Bionic.

## Ubuntu Bionic package

First make sure you have the `kframework` package installed, see instructions here: <https://github.com/kframework/k/releases>.

Download the appropriate "Ubuntu Bionic" package from the GitHub, via the [Releases](https://github.com/kframework/k/releases) page.
Releases are generated as often as possible from `master` build.
Assuming you have downloaded KIELE version `X.Y.Z`, install the package with `apt`:

```sh
sudo apt install ./kiele_X.Y.Z_amd64_bionic.deb
```

## Linux binary package

Install the following runtime dependencies:

```sh
sudo apt-get install --yes libcrypto++-dev libjemalloc-dev libmpfr-dev libprotobuf-dev libsecp256k1-dev
```

Download the "KIELE Linux Binary" package from the GitHub, via the [Releases](https://github.com/kframework/k/releases) page.
Releases are generated as often as possible from `master` build.

Assuming you have downloaded KIELE version `X.Y.Z`, extract the tarball:

```sh
tar -xvf kiele-X.Y.Z-bin.tar.gz
```

Copy all the files in the tarball into place:

```sh
mkdir -p ~/.local/bin ~/.local/lib
cp -r kiele-X.Y.Z-bin/bin/* ~/.local/bin/
cp -r kiele-X.Y.Z-bin/lib/* ~/.local/lib/
```

And make sure it's on `PATH`:

```sh
export PATH=$HOME/local/bin:$PATH
```

## Build from source

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
sudo bash -c 'OPAMROOT=/usr/lib/kframework/opamroot opam install --yes ocaml-protoc rlp yojson zarith hex uuidm cryptokit'
export OPAMROOT=/usr/lib/kframework/opamroot
eval $(opam config env)
make COVERAGE=k
```
