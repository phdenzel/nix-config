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

  # User settings
  users.mutableUsers = false;
  users.defaultUserShell = pkgs.bash;
}
