# NixOS configuration file for fenrix
{
  pkgs,
  config,
  inputs,
  ...
}: let
  hostName = "fenrix";
in {
  imports = [
    ../_common # default nix (and sops-nix) configuration
    ../_common/nfs.nix # NFS mounts
    ../_common/sddm.nix # display manager
    ../_common/hyprland.nix # window manager
    ../_common/gnome.nix # desktop as backup when Hyprland is bricked
    ../_common/fonts.nix # font packages
    ../_common/thunar.nix # file manager
    ../_common/security.nix # security configs
    ../_common/openssh.nix # openSSH configs
    ../_common/crypt-utils.nix # cryptographic tool collection
    ../_common/emacs.nix # editor and god tool
    ../_common/cli-utils.nix # cli tool collection
    ../_common/dev-utils.nix # dev tool collection
    ../_common/computing.nix # computing tool collection
    ../_common/containerization.nix # podman and oci stuff
    ../_common/graphical.nix # graphical applications
    ../_common/comm.nix # communication apps
    ../_common/tx-rx.nix # transmission / reception
    ../_common/texlive.nix # full TeXLive package
    ../_common/vpn-zhaw.nix # VPN for work
    ../../modules # AMD/Nvidia, Internationalization configs
    inputs.hardware.nixosModules.lenovo-thinkpad-t14s
    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-pc-laptop-ssd
  ];

  boot = {
    # Kernel
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = ["mem_sleep_default=deep"];
    # Bootloader
    #loader.systemd-boot.enable = true;
    #loader.systemd-boot.editor = false;
    loader.efi.canTouchEfiVariables = true;
    loader.grub.enable = true;
    loader.grub.efiSupport = true;
    loader.grub.devices = ["nodev"];
    supportedFilesystems = ["nfs"];
    # /tmp as tmpfs
    tmp = {
      useTmpfs = false;
      tmpfsSize = "5%";
      cleanOnBoot = (!config.boot.tmp.useTmpfs);
    };
    # Boot screen
    # plymouth.enable = mkDefault true;
  };

  # Suspend settings
  powerManagement.enable = true;
  services.power-profiles-daemon.enable = true;
  systemd.sleep.extraConfig = ''
    HibernateDelaySec=30m
    SuspendState=mem
  '';
  services.logind = {
    lidSwitch = "suspend-then-hibernate";
    powerKey = "hibernate";
    powerKeyLongPress = "poweroff";
  };

  # File system configuration
  services.btrfs.autoScrub = {
    enable = true;
    interval = "weekly";
    fileSystems = ["/"];
  };

  # Language customization (see ../../modules)
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  sops-host.enable = true;
  users.users.root = {
    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
    ];
  };

  # Networking
  networking = {
    hostName = "${hostName}";
    hostId = "42b816d1";
    wireless.enable = false;  # wireless via wpa_supplicant.
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
  };

  # System-wide packages
  environment.defaultPackages = [];
  environment.systemPackages = with pkgs; [
    exfat
    exfatprogs
    gparted
    lm_sensors
    networkmanagerapplet
    pavucontrol
    podman-desktop
    udiskie
    usbutils
  ];

  # System-wide programs
  programs = {
    firefox.enable = true;
    thunderbird.enable = true;
    winbox.enable = true;
    winbox.package = pkgs.winbox4;
  };

  # System-wide services
  services = {
    blueman.enable = true;
    gnome.gnome-keyring.enable = true;
    gvfs.enable = true;
    # fprintd.enable = true;
    fwupd.enable = true;
    playerctld.enable = true;
    printing.enable = true;
    printing.cups-pdf.enable = true;
    printing.drivers = with pkgs; [
      hplip
    ];
    udisks2.enable = true;
  };

  system.stateVersion = "24.11";
}
