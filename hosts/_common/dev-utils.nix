{pkgs, lib, ...}: with lib; {
  environment.systemPackages = with pkgs; [
    binutils
    bun
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
    yq-go
  ];
}
