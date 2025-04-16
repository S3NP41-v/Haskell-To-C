{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "linux-kernel-build";
  nativeBuildInputs = with pkgs; [
    getopt
    flex
    bison
    gcc
    gcc-arm-embedded
    gnumake
    bc
    pkg-config
    binutils
  ];
  buildInputs = with pkgs; [
    elfutils
    ncurses
    openssl
    zlib
  ];
}
