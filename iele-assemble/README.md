# iele-assemble

## Build instructions

### Nix

From the root of the `iele-semantics` repository,
run `nix-build -A iele-assemble`.

### Stack

To build: `stack build`.
all dependencies are managed by stack.

To run: `stack exec iele-assemble FILE`.
or `stack exec iele-assemble -- [OPTIONS] FILE`.

To run the tests: `stack test`
