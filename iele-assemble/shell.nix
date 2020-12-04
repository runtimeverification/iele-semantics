{ default ? import ./. {} }:

let
  inherit (default) project;
  inherit (project) shellFor;

  sources = import ../nix/sources.nix;
  pkgs = import sources."nixpkgs" {};

  inherit (pkgs) stack;
in

shellFor {
  buildInputs = [ stack ];
}
