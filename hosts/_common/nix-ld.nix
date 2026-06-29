# nix-ld: provides the standard ELF dynamic loader + common libraries so that
# pre-compiled, non-Nix Linux binaries (downloaded tools, pip wheels with native
# extensions, VS Code server, proprietary SDKs, ...) can run on NixOS.
# NixOS-only option; import this module ONLY from Linux hosts.
{pkgs, ...}: {
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
