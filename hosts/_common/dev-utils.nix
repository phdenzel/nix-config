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
    git-filter-repo
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
    # Declarative Rust nightly toolchain via fenix (see overlays/default.nix).
    # Replaces rustup, whose downloaded binaries broke after GC removed the
    # glibc store path they were patched against.
    (fenix.complete.withComponents [
      "cargo"
      "clippy"
      "rust-src"
      "rustc"
      "rustfmt"
    ])
    fenix.rust-analyzer
    uv
  ];
}
