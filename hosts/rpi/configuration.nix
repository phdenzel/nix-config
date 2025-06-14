# NixOS configuration file for phinix
{
  pkgs,
  lib,
  inputs,
  ...
}: let
  hostName = "rpi";
in {
  imports = [
    ../_common/nix.nix # default nix configuration
    ../_common/security.nix # security configs
    ../_common/openssh.nix # openSSH configs
    ../_common/crypt-utils.nix # cryptographic tool collection
    ../_common/emacs.nix # editor and god tool
    ../_common/cli-utils.nix # cli tool collection
    ../../modules/intl.nix # internationalization configs
    inputs.hardware.nixosModules.raspberry-pi-4
  ];

  sdImage.compressImage = lib.mkForce false;
  # Workaround for `modprobe: FATAL: Module sun4i-drm not found in directory`
  # See: https://github.com/NixOS/nixpkgs/issues/154163
  nixpkgs.overlays = [
    (final: super: {
      makeModulesClosure = x:
        super.makeModulesClosure (x // {allowMissing = true;});
    })
  ];

  boot = {
    supportedFilesystems.zfs = lib.mkForce false;
  };

  # Language customization (see ../../modules)
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  services.getty.autologinUser = "root";
  users.users.root = {
    initialHashedPassword = "";
    # for remote builds
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFyUOUvvJENjM7fNdGW/9ljjJnPEGHlt1pYFYRx2mZIW phdenzel@sol"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOr6HdrDEH1BztobKQo9xZqlqqYUEWTuz5+QricptROm phdenzel@fenrix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZofJltIURsWCGEc+H5wyp4WJ3GGkcjPR5THptcR1dg phdenzel@asahi"
    ];
  };

  # Networking
  networking = {
    hostName = "${hostName}";
    # hostId = "27b636ba";
    wireless.enable = false;
    networkmanager.enable = true;
    enableIPv6 = false;
  };
  systemd.services.NetworkManager-wait-online.enable = false;

  # Local networking
  services.avahi = {
    enable = true;
    domainName = "local";
    nssmdns4 = true;
    openFirewall = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };
  };

  # System-wide packages
  environment.defaultPackages = [];
  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
    usbutils
  ];

  # System-wide services
  services = {
    udisks2.enable = true;
  };

  system.stateVersion = "25.05";
}
