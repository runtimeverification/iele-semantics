{ checkMaterialization ? false
}:

let
  sources = import ../nix/sources.nix;

  pkgs =
    let
      haskell-nix = import sources."haskell.nix" {};
      inherit (haskell-nix) nixpkgsArgs;
      args = nixpkgsArgs // { };
    in import haskell-nix.sources.nixpkgs args;
  inherit (pkgs) lib haskell-nix;

  project = (args: haskell-nix.stackProject args) {
    inherit checkMaterialization;
    materialized = ../nix/iele-assemble.nix.d;
    src = haskell-nix.haskellLib.cleanGit { src = ./..; subDir = "iele-assemble"; };
  };

  default =
    {
      inherit pkgs project;
      inherit (project.iele-assemble.components.exes) iele-assemble;
    };

in default
