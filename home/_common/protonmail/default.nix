{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./bridge.nix
  ];

  home.packages =
    [
      pkgs.proton-pass
    ]
    # proton-vpn and the bridge GUI have no Darwin build; only install on Linux.
    ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [
      pkgs.proton-vpn
      pkgs.protonmail-bridge-gui
    ];
}
