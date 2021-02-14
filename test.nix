let
  sources = import ./nix/sources.nix;
  pinned = import sources."nixpkgs" { config = {}; overlays = []; };
in

{ pkgs ? pinned
, test ? null
}:

let
  inherit (pkgs) stdenv lib;

  ttuegel = import sources."nix-lib" { inherit pkgs; };
  default = import ./. { inherit pkgs; };
  inherit (default) kiele;
  inherit (pkgs.python2Packages) wrapPython python;
  inherit (pkgs) ncurses;

in

stdenv.mkDerivation {
  name = "iele-semantics-test";
  src = ttuegel.cleanSourceWith {
    name = "iele-semantics";
    src = ttuegel.cleanGit { name = "iele-semantics"; src = ./.; };
    ignore = [ "*.nix" "/nix" ];
  };
  preferLocalBuild = true;
  nativeBuildInputs = [ kiele (lib.getBin ncurses) python ];
  enableParallelBuilding = true;
  postPatch = ''
    patchShebangs assemble-iele-test
  '';
  configurePhase = ''
    echo $PATH
  '';
  buildFlags = [
    "test-interactive"
    "test-iele"
    "test-wellformed"
    "test-illformed"
  ];
  installPhase = ''
    runHook preInstall

    touch "$out"

    runHook postInstall
  '';
}

