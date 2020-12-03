# iele-assemble

## For users

### Install

#### Nix (Linux)

1.  **Dependencies:**
    Follow the instructions below.
    Perform all steps as your non-root user.
    You may find the same instructions and our public key at <https://runtimeverification.cachix.org>.
    It is safe to skip any dependencies that are already installed.
    1.  **Nix:** `bash <(curl -L https://nixos.org/nix/install)`.
    2.  **Cachix:** `nix-env -iA cachix -f https://cachix.org/api/v1/install`
    3.  **Binary cache:** `cachix use runtimeverification`
2.  **Install:** `nix-env -iA iele-assemble -f https://github.com/runtimeverification/iele-semantics/archive/master.tar.gz`

#### Ubuntu

...

## For developers

### Build

#### Nix

-   **Build:** `nix-build -A iele-assemble`
-   **Test:** `nix-build -A iele-assemble-project.iele-assemble.checks`

#### Stack

-   **Build:** `stack build`
-   **Test:** `stack test`
-   **Run:** `stack exec iele-assemble -- [OPTIONS] FILE`
