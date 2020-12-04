# iele-assemble

## For users

Please refer to the [installation instructions](../INSTALL.md).

## For developers

Install Nix and set up the binary cache following the [installation instructions](#nix) above.
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
