{ checkMaterialization ? false
}:

let
  sources = import ./nix/sources.nix;

  pkgs =
    let
      haskell-nix = import sources."haskell.nix" {};
      inherit (haskell-nix) nixpkgsArgs;
      args = nixpkgsArgs // { };
    in import haskell-nix.sources.nixpkgs-2003 args;
  inherit (pkgs) lib haskell-nix;

  iele-assemble-project = (args: haskell-nix.stackProject args) {
    inherit checkMaterialization;
    materialized = ./nix/iele-assemble.materialized;
    src = haskell-nix.haskellLib.cleanGit { src = ./.; subDir = "iele-assemble"; };
  };

  default =
    {
      inherit pkgs iele-assemble-project;
      inherit (iele-assemble-project.iele-assemble.components.exes) iele-assemble;
      cache = [
        iele-assemble-project.roots
      ];
    };

in default
