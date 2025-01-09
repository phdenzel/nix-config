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
  ];

  sops-user = {
    enable = true;
    user = "${userName}";
    host = "${hostName}";
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
      ];
    hashedPasswordFile = config.sops.secrets."passwd/${userName}/${hostName}".path;
    # openssh.authorizedKeys.keys = [];
    packages = with pkgs; [home-manager];
  };
}
