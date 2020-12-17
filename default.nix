{}:

let
  sources = import ./nix/sources.nix {};
  pkgs = import sources."nixpkgs" {};
  inherit (pkgs.lib) importJSON;
  kframework =
    let
      src = pkgs.fetchgit {
        inherit (importJSON ./nix/k.lock.json)
          url rev sha256 fetchSubmodules deepClone leaveDotGit;
      };
    in import src {};
  inherit (kframework) k haskell-backend llvm-backend clang;
  llvmPackages = pkgs.llvmPackages_10;
in

let
  inherit (pkgs) callPackage;
in

let
  libff = callPackage ./nix/libff.nix {
    stdenv = llvmPackages.stdenv;
  };
  kiele = callPackage ./nix/kiele.nix {
    inherit libff;
    inherit k haskell-backend llvm-backend clang;
  };
  iele-assemble = import ./iele-assemble {};

  default =
    {
      inherit kiele;
      inherit (iele-assemble) iele-assemble;
    };

in default
