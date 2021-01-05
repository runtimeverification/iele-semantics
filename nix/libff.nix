{ stdenv, cmake, pkgconfig, gmp, openssl, procps }:

stdenv.mkDerivation {
  name = "libff";
  src = ../plugin/deps/libff;
  nativeBuildInputs = [ cmake pkgconfig ];
  propagatedBuildInputs = [ gmp openssl procps ];
}