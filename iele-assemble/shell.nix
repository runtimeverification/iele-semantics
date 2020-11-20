{ default ? import ../default.nix {} }:

let
  inherit (default) iele-assemble-project;
  inherit (iele-assemble-project) shellFor;

  sources = import ../nix/sources.nix;
  pkgs = import sources."nixpkgs" {};

  inherit (pkgs) stack;
in

shellFor {
  buildInputs = [ stack ];
}
