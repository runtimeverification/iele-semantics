{ stdenv, wrapPython, src, cleanSourceWith
, protobuf
, cryptopp, libff, mpfr, secp256k1
, jemalloc, libffi, ncurses
, k, haskell-backend, llvm-backend, clang, python
}:

stdenv.mkDerivation {
  name = "kiele-0.2.0";
  src = cleanSourceWith {
    name = "iele-semantics";
    inherit src;
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
  nativeBuildInputs = [
    protobuf python wrapPython
    k haskell-backend llvm-backend clang
  ];
  buildInputs = [
    cryptopp libff mpfr secp256k1
    # TODO: propagate from llvm-backend:
    jemalloc libffi ncurses
  ];
  makeFlags = [
    "libff_out=${libff}/lib/libff.a"
    "INSTALL_PREFIX=${builtins.placeholder "out"}"
  ];
  buildFlags = [
    "build-interpreter"
  ];
  preBuild = ''
    sed -i kiele \
      -e "/^INSTALL_BIN=/ c INSTALL_BIN=\"$out/bin\"" \
      -e "/^export LD_LIBRARY_PATH=/ d"
    patchPythonScript kore-json.py
  '';
  installTargets = [
    "install-kiele"
  ];
}