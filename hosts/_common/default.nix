# Common configuration for all hosts
{
  pkgs,
  lib,
  inputs,
  outputs,
  ...
}: let
  flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
in {
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];

  nix = {
    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = ["root" "@wheel" "phdenzel"];
      auto-optimise-store = lib.mkDefault true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than +5";
    };
    optimise.automatic = true;

    registry = lib.mapAttrs (_: flake: {inherit flake;}) flakeInputs;
    nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}") flakeInputs;
  };

  hardware.enableRedistributableFirmware = true;

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config = {
      allowUnfree = true;
    };
  };

  home-manager.useGlobalPkgs = true;
  # home-manager.useUserPackages = true;
  home-manager.extraSpecialArgs = {
    inherit inputs outputs;
  };

  users.defaultUserShell = pkgs.bash;
}
