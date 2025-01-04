{
  inputs,
  outputs,
  pkgs,
  config,
  ...
}: let
  ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
in {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    ./home.nix
    ../_common/sops.nix
  ];

  # User settings
  home-manager.useGlobalPkgs = true;
  # home-manager.useUserPackages = true;
  home-manager.extraSpecialArgs = {
    inherit inputs outputs;
  };

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
  };
}
