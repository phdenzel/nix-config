# NixOS configuration file for idun
{
  pkgs,
  config,
  ...
}: let
  hostName = "idun";
in {
  imports = [
    ../_common # Default nix (and sops-nix) configuration
    ../_common/sddm.nix # Display manager
    ../_common/hyprland.nix # Window manager
    ../_common/gnome.nix # Desktop as backup when Hyprland is bricked
    ../_common/thunar.nix # File manager
    ../_common/openssh.nix # OpenSSH
    ../_common/crypt-utils.nix # Cryptographic tool compilation
    ../_common/cli-utils.nix # CLI tool compilation
    ../../modules # AMD/Nvidia, Internationalization configs
  ];

  boot = {
    # Kernel
    kernelPackages = pkgs.linuxPackages;
    # kernelPackages = pkgs.linuxPackages_latest;
    # kernelPackages = pkgs.linuxPackages_zen;

    # Bootloader
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    # /tmp as tmpfs
    tmp = {
      useTmpfs = true;
      tmpfsSize = "5%";
    };

    # Boot screen
    # plymouth.enable = mkDefault true;
  };

  # Custom modules (see ../../modules)
  drivers.amdgpu.enable = false;
  drivers.amdgpu.utils.install = false;
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  users.users.root = {
    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
  };

  # Networking
  networking.hostName = "${hostName}";
  # networking.wireless.enable = true;  # wireless via wpa_supplicant.
  networking.networkmanager.enable = true;
  # systemd.services.NetworkManager-wait-online.enable = false;

  # System-wide packages
  environment.defaultPackages = [];

  # System-wide services
  services = {
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
    btrfs.autoScrub.enable = true;
  };

  # VM specifics
  services.spice-vdagentd.enable = true;

  # Security
  security.rtkit.enable = true; # recommended for pipewire
  security.polkit.enable = true;

  system.stateVersion = "24.11";
}
