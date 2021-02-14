{ stdenv, lib, wrapPython, src, cleanSourceWith
, protobuf
, cryptopp, libff, mpfr, secp256k1
, jemalloc, libffi, ncurses
, k, haskell-backend, llvm-backend, clang, python
, iele-assemble
}:

let src' = src; in

let
  version = "0.2.0";

  src = cleanSourceWith {
    name = "iele-semantics";
    src = src';
    ignore = [
      "*.nix"
      "/iele-assemble"
      "/iele-examples"
      "/nix"
      "/package"
      "/tests"
      "/web"
    ];
  };

  mkIELE = target: f: stdenv.mkDerivation (f {
    pname = "iele-${target}";
    inherit version src;
    nativeBuildInputs = [ protobuf k haskell-backend llvm-backend clang ];
    buildInputs = [
      cryptopp libff mpfr secp256k1
      # TODO: propagate from llvm-backend:
      jemalloc libffi ncurses
    ];
    makeFlags = [
      "libff_out=${libff}/lib/libff.a"
      "INSTALL_PREFIX=${builtins.placeholder "out"}"
    ];
    buildFlags = [ "build-${target}" ];
    installTargets = [ "install-${target}" ];
  });

  iele-interpreter = mkIELE "interpreter" (x: x);

  iele-vm = mkIELE "vm" (x: x);

  iele-check = mkIELE "check" (x: x);

in

stdenv.mkDerivation {
  pname = "kiele";
  inherit version src;
  nativeBuildInputs = [ k python wrapPython ];
  makeFlags = [ "INSTALL_PREFIX=${builtins.placeholder "out"}" ];
  postPatch = ''
    sed -i kiele \
      -e "/^INSTALL_BIN=/ c INSTALL_BIN=\"$out/bin\"" \
      -e "/^export LD_LIBRARY_PATH=/ d"

    patchPythonScript kore-json.py
  '';
  buildPhase = "true";
  installTargets = [ "install-kiele" ];
  postInstall = ''
    ln -s ${lib.getBin iele-assemble}/bin/iele-assemble $out/bin
    ln -s ${lib.getBin iele-check}/bin/iele-check $out/bin
    ln -s ${lib.getBin iele-interpreter}/bin/iele-interpreter $out/bin
    ln -s ${lib.getBin iele-vm}/bin/iele-vm $out/bin

    ln -s ${lib.getLib iele-interpreter}/lib/kiele/standalone $out/lib/kiele
  '';
  passthru = { inherit iele-assemble iele-check iele-interpreter iele-vm; };
}
