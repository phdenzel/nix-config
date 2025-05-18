# NixOS configuration file for phinix
{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: let
  hostName = "heimdall";
in {
  imports = [
    ../_common/nix.nix # default nix configuration
    ../_common/sops.nix # host secrets
    ../_common/security.nix # security configs
    ../_common/openssh.nix # openSSH configs
    ../_common/crypt-utils.nix # cryptographic tool collection
    ../_common/emacs.nix # editor and god tool
    ../_common/cli-utils.nix # cli tool collection
    ../_srv/blocky.nix # DNS server
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
    # /tmp as tmpfs
    tmp = {
      useTmpfs = false;
      tmpfsSize = "50%";
      cleanOnBoot = (!config.boot.tmp.useTmpfs);
    };
  };

  # Language customization (see ../../modules)
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  sops-host.enable = true;
  users.users.root = {
    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
    # for remote builds
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOr6HdrDEH1BztobKQo9xZqlqqYUEWTuz5+QricptROm phdenzel@fenrix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZofJltIURsWCGEc+H5wyp4WJ3GGkcjPR5THptcR1dg phdenzel@asahi"
    ];
  };

  # Networking
  networking = {
    hostName = "${hostName}";
    hostId = "78e00dd5";
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
    exfat
    exfatprogs
    usbutils
  ];

  # System-wide services
  services = {
    udisks2.enable = true;
  };

  system.stateVersion = "25.05";
}
