{
  pkgs,
  config,
  inputs,
  ...
}: let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
  userName = "phdenzel";
  hostName = "sol";
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
    gpgKeys = ["pwds"];
    genericKeys = [
      "syncthing/${userName}/${hostName}/password"
      "syncthing/${userName}/${hostName}/cert.pem"
      "syncthing/${userName}/${hostName}/key.pem"
    ];
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
        "disk"
        "docker"
        "git"
        "grafana"
        "input"
        "jupyter"
        "kvm"
        "libvirtd"
        "network"
        "networkmanager"
        "openrazer"
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
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOr6HdrDEH1BztobKQo9xZqlqqYUEWTuz5+QricptROm phdenzel@fenrix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPb8EAGxZ/djOy8zy3tTUBYl45LgP0oitoL099hNtMbj phdenzel@ygdrasil"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZofJltIURsWCGEc+H5wyp4WJ3GGkcjPR5THptcR1dg phdenzel@asahi"
    ];
  };

  home-manager.users.${userName} = {
    imports = [
      ./_configs/gpg/key-pwds.nix
      ./_configs/gpg/gpg.nix
      ./_configs/gpg/agent.nix
    ];
    services.syncthing = {
      passwordFile = "${config.sops.secrets."syncthing/${userName}/${hostName}/password".path}";
      key = "${config.sops.secrets."syncthing/${userName}/${hostName}/key.pem".path}";
      cert = "${config.sops.secrets."syncthing/${userName}/${hostName}/cert.pem".path}";
    };
    # wayland.windowManager.hyprland.settings.monitor = [
    #   "HDMI-1, 3840x1080@60.0, 0x0, 1"
    # ];
    stylix = {
      image = (inputs.phd-wallpapers + ./noctilusent_4k.png);
      imageScalingMode = "fill";
    };
    services.hyprpaper.settings.wallpaper = [
      {
        monitor = "";
        path = "/etc/wallpapers/noctilusent_4k.png";
      }
    ];
  };
}
