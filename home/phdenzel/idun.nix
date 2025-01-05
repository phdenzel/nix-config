{
  pkgs,
  config,
  ...
}: let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
in {
  imports = [
    ./home.nix
    ../_common/sops.nix
  ];

  users.users.phdenzel = {
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

  home-manager.users.phdenzel =
    import ./${config.networking.hostName}.nix;

}
