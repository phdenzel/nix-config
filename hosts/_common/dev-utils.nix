{
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    bacon
    binutils
    bun
    cargo-show-asm
    cargo-license
    cargo-outdated
    cmake
    gdb
    gcc
    gfortran
    gnumake
    gnuplot
    jq
    libgcc
    libgccjit
    pkg-config
    podman
    podman-compose
    python312
    (python312.withPackages (p: with p; [
      pip
      virtualenv
      isort
      jedi
      mypy
      python-lsp-server
      rope
      ruff
      numpy
    ]))
    python313
    python311
    python310
    rustup
    uv
    yq-go
  ];
  
}
