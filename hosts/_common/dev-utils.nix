# Cross-platform dev tool collection.
# Linux-only extras live in: dev-linux.nix
{
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    bacon
    binutils
    bun
    cargo-flamegraph
    cargo-license
    cargo-outdated
    cargo-show-asm
    clang-tools
    cmake
    gcc
    gfortran
    gnumake
    gnuplot
    hdf5
    nodejs
    pkg-config
    (python313.withPackages (p: with p; [
      pip
      virtualenv
      isort
      jedi
      mypy
      python-lsp-server
      rope
      ruff
    ]))
    rustup
    uv
  ];
}
