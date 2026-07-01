# Per-host user + home-manager config for the darwin host `asahi`.
{
  pkgs,
  inputs,
  lib,
  system,
  ...
}: let
  userName = "phdenzel";
  hostName = "asahi";
in {
  imports = [
    inputs.sops-nix.darwinModules.sops
    ../_common/sops.nix
    inputs.home-manager.darwinModules.home-manager
  ];

  sops-user = {
    enable = true;
    user = "${userName}";
    host = "${hostName}";
    usersRoot = "/Users";
    gpgKeys = ["pwds"];
  };

  users.users.${userName} = {
    name = "${userName}";
    home = "/Users/${userName}";
    shell = pkgs.zsh;
  };

  home-manager = {
    extraSpecialArgs = {inherit inputs system;};
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${userName} = {
      imports = [
        ./default.nix # shared cross-platform home config
        ./_configs/gpg/gpg.nix
        ./_configs/gpg/key-pwds.nix
      ];
      programs.gpg.settings.default-key = lib.mkDefault "629FC7317EFB4935";
    };
  };
}
