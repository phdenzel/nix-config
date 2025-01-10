{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    bacon
    binutils
    bun
    cargo-asm
    cargo-license
    cargo-outdated
    cmake
    gdb
    gcc
    gfortran
    gnumake
    gnuplot
    jq
    libgccjit
    pkg-config
    podman
    podman-compose
    rustup
    yq-go
  ];
}
