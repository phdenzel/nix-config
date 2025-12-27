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
    cmake
    gdb
    gcc
    gfortran
    gnumake
    gnuplot
    hdf5
    perf
    pkg-config
    podman
    podman-compose
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
    python312
    python311
    python310
    rustup
    uv
  ];
  programs = {
    java.enable = true;
    java.binfmt = true;
  };

  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [
      acl
      bzip2
      curl
      dbus
      freetype
      libgcc
      libgccjit
      libGL
      libssh
      libuuid
      libxml2
      openblas
      lapack
      openssl
      stdenv.cc.cc
      util-linux
      xz
      zlib
      zstd
    ];
  };
  
}
