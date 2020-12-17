{ stdenv, nix-gitignore
, protobuf
, cryptopp, libff, mpfr, secp256k1
, jemalloc, libffi, ncurses
, k, haskell-backend, llvm-backend, clang
}:

stdenv.mkDerivation {
  name = "kiele-0.2.0";
  src =
    let
      path = ./..;
      patterns = [
        "/.build"
        "/tests"
        "result*"
        "/iele-assemble"
        "/web"
      ];
      inherit (nix-gitignore) gitignoreFilterPure;
    in
      builtins.path {
        inherit path;
        name = "iele-semantics";
        filter = gitignoreFilterPure (_: _: true) patterns path;
      };
  nativeBuildInputs = [
    protobuf
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
    "build-vm"
  ];
  installTargets = [
    "install-interpreter"
    "install-vm"
  ];
}