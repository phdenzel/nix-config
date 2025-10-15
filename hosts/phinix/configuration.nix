# NixOS configuration file for phinix
{
  pkgs,
  config,
  inputs,
  ...
}: let
  hostName = "phinix";
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
    ../_common/ollama.nix # local LLM services
    ../_common/graphical.nix # graphical applications
    # ../_common/ghostfolio.nix # wealth management tool
    ../_common/comm.nix # communication apps
    ../_common/tx-rx.nix # transmission / reception
    ../_common/mux.nix # muxing tool collection
    ../_common/games.nix # Gaming utils
    ../_common/texlive.nix # full TeXLive package
    ../_common/vpn-zhaw.nix # VPN for work
    # testing
    ../_srv/dashboards.nix
    ../_srv/blocky.nix
    ../_srv/blocky-grafana.nix
    # ../_srv/jellyfin.nix
    # ../_srv/cloud.nix
    # ../_srv/forgejo.nix
    # ../_srv/immich.nix
    # ../_srv/vikunja.nix
    # ../_srv/mealie.nix
    
    ../../modules # Internationalization configs
    inputs.hardware.nixosModules.common-cpu-amd-pstate
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd
  ];

  boot = {
    # Kernel
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = ["sg"];  # for disk drives
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
      useTmpfs = true;
      tmpfsSize = "5%";
      cleanOnBoot = (!config.boot.tmp.useTmpfs);
    };
    # Cross building
    binfmt.emulatedSystems = [
      "aarch64-linux"
    ];
    # Boot screen
    # plymouth.enable = mkDefault true;
  };

  # File system configuration
  services.btrfs.autoScrub = {
    enable = true;
    interval = "weekly";
    fileSystems = ["/" "/raid" "/scratch"];
  };

  # Hardware customization
  nixpkgs.config.rocmSupport = true;
  services.ollama.acceleration = "rocm";
  # systemd.tmpfiles.rules = with pkgs;
  #   mkDefault [
  #     "L+    /opt/rocm/hip   -    -    -     -    ${rocmPackages.clr}"
  #   ];

  # Language customization (see ../../modules)
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  sops-host = {
    enable = true;
    keys = [
      "homepage-dashboard/env"
    ];
    ownedKeys = [
      "grafana/admin_password"
    ];
  };
  users.users.root = {
    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
    # openssh.authorizedKeys.keys = [
    #   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
    # ];
  };

  # Networking
  networking = {
    hostName = "${hostName}";
    hostId = "27b636ba";
    # wireless.enable = true;  # wireless via wpa_supplicant.
    networkmanager.enable = true;
    enableIPv6 = false;
  };

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
    btop-rocm
    caligula
    clinfo
    exfat
    exfatprogs
    # fancontrol-gui
    gparted
    jabref
    lact
    libva-utils
    lm_sensors
    networkmanagerapplet
    pavucontrol
    podman-desktop
    polychromatic
    razergenie
    rocmPackages.rocm-smi
    rocmPackages.rocminfo
    stable.rgp
    vdpauinfo
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
    fwupd.enable = true;
    hardware.openrgb.enable = true;
    hardware.openrgb.package = pkgs.openrgb-with-all-plugins;
    playerctld.enable = true;
    printing.enable = true;
    printing.cups-pdf.enable = true;
    printing.drivers = with pkgs; [
      hplip
    ];
    udisks2.enable = true;
  };
  
  # Hardware
  hardware = {
    openrazer.enable = true;
    openrazer.batteryNotifier.enable = true;
    # uni-sync.enable = true;
    # fancontrol.enable = true;
    # fancontrol.config = {};
  };

  system.stateVersion = "24.11";
}
