{
  pkgs,
  config,
  ...
}: let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
  userName = "phdenzel";
in {
  imports = [
    ../_common/sops.nix
  ];

  sops-config = {
    enable = true;
    name = "${userName}";
  };

  users.users."${userName}" = {
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
    hashedPasswordFile = config.sops.secrets."passwd/idun".path;
    # openssh.authorizedKeys.keys = [];
    packages = with pkgs; [home-manager];
  };

  home-manager.users."${userName}" =
    import ./${config.networking.hostName}.nix;

}
