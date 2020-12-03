# iele-assemble

## For users

### Install

#### Nix (Linux)

##### Prerequisites

It is safe to skip any of these dependencies that are already installed.

Follow the instructions below.
Perform all steps as your normal (non-root) user.
You may find the same instructions and our public key at <https://runtimeverification.cachix.org>.

1.  **Curl:** Install `curl` using your distribution's package manager.
    -  **Ubuntu:** `sudo apt install curl`
2.  **Nix:** `bash <(curl -L https://nixos.org/nix/install)`.
    Please pay careful attention to any additional instructions printed by the installer.
3.  **Cachix:** `nix-env -iA cachix -f https://cachix.org/api/v1/install`
4.  **Binary cache:** `cachix use runtimeverification`

##### Install

```
nix-env -iA iele-assemble -f https://github.com/runtimeverification/iele-semantics/archive/master.tar.gz
```

#### Ubuntu

...

## For developers

### Build

#### Nix

**Prerequisites:** Install Nix and set up the binary cache following the [instructions](#prerequisites) above.

-   **Build:** `nix-build -A iele-assemble`
-   **Test:** `nix-build -A iele-assemble-project.iele-assemble.checks`

#### Stack

**Prerequisites:** Install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

-   **Build:** `stack build`
-   **Test:** `stack test`
-   **Run:** `stack exec iele-assemble -- [OPTIONS] FILE`
