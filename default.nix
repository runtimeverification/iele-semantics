let
  sources = import ./nix/sources.nix {};
  pinned = import sources."nixpkgs" {};
in

{ pkgs ? pinned
, release ? null
}:

let release_ = release; in

let
  inherit (pkgs) lib;
  ttuegel = import sources."nix-lib" { inherit pkgs; };

  release = if release_ == null then pkgs.stdenv.isLinux else false;

  kframework =
    let
      tag = lib.fileContents ./deps/k_release;
      url = "https://github.com/kframework/k/releases/download/${tag}/release.nix";
      args = import (builtins.fetchurl { inherit url; });
      src = pkgs.fetchgit args;
    in import src { inherit release; };
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
    src = ttuegel.cleanGitSubtree {
      name = "libff";
      src = ./.;
      subDir = "plugin/deps/libff";
    };
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
