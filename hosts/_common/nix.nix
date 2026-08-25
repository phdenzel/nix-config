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
      experimental-features = ["nix-command" "flakes"];
      trusted-users = ["root" "@wheel" "phdenzel"];
      auto-optimise-store = lib.mkDefault true;
      download-buffer-size = 524288000;

      # Build off /tmp: with boot.tmp.useTmpfs the build tree lives in RAM,
      # so a big rebuild competes with the compilers for memory. This must not
      # sit under a world-writable parent (/var/tmp is 1777) -- Nix rejects
      # that outright. Nix's own build dir is on the same btrfs subvolume as
      # the store, so finished builds are renamed in, not copied.
      build-dir = "/nix/var/nix/builds";
      # Conservative defaults; override per host (see hosts/sol).
      max-jobs = lib.mkDefault 4;
      cores = lib.mkDefault 4;

      # binary caches
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://hyprland.cachix.org"
        "https://lens-forge.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "lens-forge.cachix.org-1:piAThOyt1XYoV2nnyfpj2D6egL9t9CKh33g2692sHRw="
      ];
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 5d";
      persistent = true;
    };
    optimise.automatic = true;

    registry = lib.mapAttrs (_: flake: {inherit flake;}) flakeInputs;
    nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}") flakeInputs;
  };

  # Cage the builders: builds are forked from nix-daemon, so a limit on its
  # cgroup makes a runaway build get OOM-killed instead of freezing the box.
  systemd.services.nix-daemon.serviceConfig = {
    MemoryHigh = lib.mkDefault "75%";
    MemoryMax = lib.mkDefault "95%";
  };

  # Last-resort safety net: systemd-oomd only manages user slices, and the
  # kernel OOM killer arrives long after the desktop has stopped responding.
  services.earlyoom = {
    enable = lib.mkDefault true;
    freeMemThreshold = 5;
    freeSwapThreshold = 10;
    extraArgs = [
      "--avoid"
      "^(Hyprland|sddm|systemd|dbus-daemon|sshd|nix-daemon)$"
      "--prefer"
      "^(cc1|cc1plus|ld|ld\\.lld|lld|rustc|nvcc|hipcc|clang|clang\\+\\+|ninja)$"
    ];
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
