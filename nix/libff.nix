{ stdenv, cmake, pkgconfig, gmp, openssl, procps, src }:

stdenv.mkDerivation {
  name = "libff";
  inherit src;
  nativeBuildInputs = [ cmake pkgconfig ];
  propagatedBuildInputs = [ gmp openssl procps ];
}