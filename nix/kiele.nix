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
        "result*"
        "/iele-assemble"
        "/tests"
        "/web"
      ];
      inherit (nix-gitignore) gitignoreFilterPure withGitignoreFile;
      filter =
        gitignoreFilterPure
          (_: _: true)
          (withGitignoreFile patterns path)
          path;
    in
      builtins.path {
        inherit path filter;
        name = "iele-semantics";
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