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
    ../_common/sddm.nix # display manager
    ../_common/hyprland.nix # window manager
    ../_common/gnome.nix # desktop as backup when Hyprland is bricked
    ../_common/fonts.nix # font packages
    ../_common/thunar.nix # file manager
    ../_common/openssh.nix # openSSH
    ../_common/crypt-utils.nix # cryptographic tool collection
    ../_common/emacs.nix # editor and god tool
    ../_common/cli-utils.nix # cli tool collection
    ../_common/dev-utils.nix # dev tool collection
    ../_common/computing.nix # computing tool collection
    ../_common/ollama.nix # local LLM services
    ../_common/graphical.nix # graphical applications
    ../_common/comm.nix # communication apps
    ../_common/tx-rx.nix # transmission / reception
    ../_common/mux.nix # muxing tool collection
    ../_common/texlive.nix # full TeXLive package
    ../../modules # AMD/Nvidia, Internationalization configs
    inputs.hardware.nixosModules.common-cpu-amd
    inputs.hardware.nixosModules.common-cpu-amd-pstate
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd
  ];

  boot = {
    # Kernel
    kernelPackages = pkgs.linuxPackages_latest;
    # Bootloader
    #loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    loader.grub.enable = true;
    loader.grub.efiSupport = true;
    loader.grub.devices = ["nodev"];
    # /tmp as tmpfs
    tmp = {
      useTmpfs = true;
      tmpfsSize = "5%";
    };
    # Boot screen
    # plymouth.enable = mkDefault true;
  };

  # File system configuration
  services.btrfs.autoScrub = {
    enable = true;
    interval = "weekly";
    fileSystems = ["/"];
  };

  # Hardware customization (see ../../modules)
  drivers.amdgpu.enable = true;
  drivers.amdgpu.utils.install = true;
  nixpkgs.config.rocmSupport = true;
  services.ollama.acceleration = "rocm";

  # Language customization (see ../../modules)
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  users.users.root = {
    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
  };

  # Networking
  networking = {
    hostName = "${hostName}";
    hostId = "27b636ba";
    # wireless.enable = true;  # wireless via wpa_supplicant.
    networkmanager.enable = true;
    enableIPv6 = false;
  };
  systemd.services.NetworkManager-wait-online.enable = false;
  
  # Local networking
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # System-wide packages
  environment.defaultPackages = [];
  environment.systemPackages = with pkgs; [
    lact
    lm_sensors
    networkmanagerapplet
    pavucontrol
    podman-desktop
    rgp
    udiskie
    usbutils
  ];

  # System-wide programs
  programs = {
    firefox.enable = true;
    java.enable = true;
    java.binfmt = true;
    thunderbird.enable = true;
    winbox.enable = true;
    winbox.package = pkgs.winbox4;
  };

  # System-wide services
  services = {
    blueman.enable = true;
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
  # hardware = {
  #   uni-sync.enable = true;
  #   fancontrol.enable = true;
  #   fancontrol.config = {};
  # };

  # Security
  security.polkit.enable = true;

  system.stateVersion = "24.11";
}
