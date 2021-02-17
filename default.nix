let
  sources = import ./nix/sources.nix {};
  pinned = import sources."nixpkgs" {};
in

{ pkgs ? pinned }:

let
  ttuegel = import sources."nix-lib" { inherit pkgs; };
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
  src = ttuegel.cleanGitSubtree {
    name = "iele-semantics";
    src = ./.;
  };
  libff = callPackage ./nix/libff.nix {
    stdenv = llvmPackages.stdenv;
  };
  kiele = callPackage ./nix/kiele.nix {
    inherit src;
    inherit (ttuegel) cleanSourceWith;
    inherit libff;
    inherit k haskell-backend llvm-backend clang;
    inherit (pkgs.python2Packages) python;
    inherit (iele-assemble) iele-assemble;
  };
  iele-assemble = import ./iele-assemble {};

  default =
    {
      inherit kiele;
      inherit (iele-assemble) iele-assemble;
    };

in default
