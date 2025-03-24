{
  pkgs,
  config,
  inputs,
  ...
}: let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
  userName = "phdenzel";
  hostName = "phinix";
in {
  imports = [
    inputs.sops-nix.nixosModules.sops
    ../_common/sops.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  sops-user = {
    enable = true;
    user = "${userName}";
    host = "${hostName}";
    sshKeys = ["id_ed25519" "gh_id_ed25519" "gl_id_ed25519" "dgx_id_ed25519" "ghzhaw_id_ed25519"];
    gpgKeys = ["pwds" "gh" "pm" "ecc"];
  };

  users.users.${userName} = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups =
      [
        "audio"
        "video"
        "wheel"
      ]
      ++ ifTheyExist [
        "docker"
        "git"
        "input"
        "kvm"
        "libvirtd"
        "network"
        "networkmanager"
        "podman"
        "storage"
        "transmission"
      ];
    hashedPasswordFile = config.sops.secrets."passwd/${userName}/${hostName}".path;
    packages = with pkgs; [
      home-manager
      protonmail-desktop
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZofJltIURsWCGEc+H5wyp4WJ3GGkcjPR5THptcR1dg phdenzel@asahi"
    ];
  };

  home-manager.users.${userName}.wayland.windowManager.hyprland.settings.monitor = [
    "DP-3, 7680x2160@120.0, 0x0, 2"
  ];
}
