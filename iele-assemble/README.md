# iele-assemble

## For users

### Install - Nix

A Nix expression is provided, with binary caching for Linux and macOS.

#### Prerequisites

It is safe to skip any of these dependencies that are already installed.

Follow the instructions below.
Perform all steps as your normal (non-root) user.
You may find the same instructions and our public key at <https://runtimeverification.cachix.org>.

##### curl

Install curl using your distribution's package manager:

```.sh
# Ubuntu and Debian
sudo apt install curl

# Fedora, RHEL, and CentOS
sudo yum install curl

# Arch Linux
sudo pacman -Sy curl
```

##### Nix

Please pay careful attention to any additional instructions printed by the installer.
Install Nix:

```.sh
# Linux and macOS < 10.15
bash <(curl -L https://nixos.org/nix/install)

# macOS 10.15 (See note below)
bash <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
```

For macOS 10.15, please refer to the [Nix manual](https://nixos.org/manual/nix/stable/#sect-macos-installation) for more information.

##### Cachix

Install Cachix and start using the binary cache:

```.sh
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use runtimeverification
```

#### Install iele-assemble

With the [prerequisites](#prerequisites) installed, we are ready to install `iele-assemble`:

```.sh
nix-env -iA iele-assemble -f https://github.com/runtimeverification/iele-semantics/archive/master.tar.gz
```

### Install - Ubuntu Bionic

We provide [Ubuntu packages](https://github.com/runtimeverification/iele-semantics/releases) for `iele-assemble` alongside the other components of KIELE.
Download the package for your distribution and double-click to open it in the Software Center, or run:

```.sh
sudo apt install kiele_0.1.0_amd64_bionic.deb
```

### Install - Linux

We provide [distribution-independent binary packages](https://github.com/runtimeverification/iele-semantics/releases) for `iele-assemble` alongside the other components of KIELE.
Download and unpack the `KIELE Linux Binary` archive.

## For developers

Install Nix and set up the binary cache following the [instructions](#prerequisites) above.
You must also set up the [binary cache](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/#setting-up-the-binary-cache) for `haskell.nix`.

If you change the `.cabal` file, you must rematerialize the Nix expressions:

```.sh
# Requires Nix is installed
./nix/rematerialize.sh
```

### Build - Nix

-   **Build:** `nix-build -A iele-assemble`
-   **Test:** `nix-build -A project.iele-assemble.checks`

### Build - Stack

Install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

-   **Build:** `stack build`
-   **Test:** `stack test`
-   **Run:** `stack exec iele-assemble -- [OPTIONS] FILE`
