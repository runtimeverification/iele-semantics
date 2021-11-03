{ stdenv, lib, src, cleanSourceWith
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
    inherit version;
    src = cleanSourceWith {
      name = "iele-semantics";
      inherit src;
    };
    nativeBuildInputs = [ protobuf k haskell-backend llvm-backend clang ];
    buildInputs = [ cryptopp libff mpfr secp256k1 ];
    makeFlags =
      [
        "INSTALL_PREFIX=${builtins.placeholder "out"}"
        "SYSTEM_LIBFF=true"
        "SYSTEM_LIBSECP256K1=true"
        "SYSTEM_LIBCRYPTOPP=true"
      ];
    buildFlags = [ "build-${target}" ];
    installTargets = [ "install-${target}" ];
    buildPhase = ''
      K_OPTS=-Xmx6G make
    '';
  });

  iele-interpreter = mkIELE "interpreter" (x: x);

  iele-vm = mkIELE "vm" (x: x);

  iele-check = mkIELE "check" (x: x);

  host-PATH = lib.makeBinPath [ k llvm-backend haskell-backend ];

in

stdenv.mkDerivation {
  pname = "kiele";
  inherit version src;
  nativeBuildInputs = [ k python ];
  makeFlags = [ "INSTALL_PREFIX=${builtins.placeholder "out"}" ];
  postPatch = ''
    sed -i kiele \
      -e "/^INSTALL_BIN=/ c INSTALL_BIN=\"$out/bin\"" \
      -e "/^export LD_LIBRARY_PATH=/ d" \
      -e '2 i export PATH="${host-PATH}:$PATH"'

    patchShebangs kore-json.py
  '';
  buildFlags = [ "build-kiele" ];
  installTargets = [ "install-kiele" ];
  postInstall = ''
    ln -s ${lib.getBin iele-assemble}/bin/iele-assemble $out/bin

    ln -s ${lib.getLib iele-interpreter}/lib/kiele/standalone $out/lib/kiele
    ln -s ${lib.getLib iele-check}/lib/kiele/check $out/lib/kiele
    ln -s ${lib.getLib iele-vm}/lib/kiele/node $out/lib/kiele
  '';
  passthru = { inherit iele-assemble iele-check iele-interpreter iele-vm; };
}
