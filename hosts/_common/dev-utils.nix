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
    pkg-config
    podman
    podman-compose
    (python312.withPackages (p: with p; [
      pip
      virtualenv
      isort
      jedi
      mypy
      python-lsp-server
      rope
      ruff
    ]))
    python313
    python311
    python310
    rustup
    uv
    yq-go
  ];

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
      libssh
      libuuid
      libxml2
      openssl
      stdenv.cc.cc
      util-linux
      xz
      zlib
      zstd
    ];
  };
  
}
