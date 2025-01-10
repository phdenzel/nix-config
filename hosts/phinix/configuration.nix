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
    ../_common # Default nix (and sops-nix) configuration
    ../_common/sddm.nix # Display manager
    ../_common/hyprland.nix # Window manager
    ../_common/gnome.nix # Desktop as backup when Hyprland is bricked
    ../_common/thunar.nix # File manager
    ../_common/openssh.nix # OpenSSH
    ../_common/crypt-utils.nix # Cryptographic tool collection
    ../_common/cli-utils.nix # CLI tool collection
    ../_common/dev-utils.nix # Dev tool collection
    ../_common/emacs.nix # Editor and god tool
    ../_common/fonts.nix # Font packages
    ../_common/texlive.nix # Full TeXLive package
    ../_common/comm.nix # Communication apps
    ../../modules # AMD/Nvidia, Internationalization configs
    inputs.hardware.nixosModules.common-cpu-amd
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd
  ];

  boot = {
    # Kernel
    kernelPackages = pkgs.linuxPackages_latest;
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
  
  # File system configuration
  services.btrfs.autoScrub = {
    enable = true;
    interval = "weekly";
    fileSystems = ["/"];
  };

  # Hardware customization (see ../../modules)
  drivers.amdgpu.enable = true;
  drivers.amdgpu.utils.install = true;

  # Language customization (see ../../modules)
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  users.users.root = {
    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
  };

  # Networking
  networking.hostName = "${hostName}";
  #networking.wireless.enable = true;  # wireless via wpa_supplicant.
  networking.networkmanager.enable = true;
  #systemd.services.NetworkManager-wait-online.enable = false;
  # Local networking
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # System-wide packages
  environment.defaultPackages = [];
  environment.systemPackages = with pkgs; [
    filezilla
    gimp
    inkscape
    lact
    imv
    libreoffice-fresh
    mpv
    pavucontrol
    podman-desktop
    protonmail-desktop
    rgp
    udiskie
    zathura
  ];

  # System-wide programs
  programs = {
    firefox.enable = true;
    java.enable = true;
    java.binfmt = true;
    thunderbird.enable = true;
    winbox.enable = true;
  };

  # System-wide services
  services = {
    blueman.enable = true;
    gvfs.enable = true;
    fwupd.enable = true;
    hardware.openrgb.enable = true;
    hardware.openrgb.package = pkgs.openrgb-with-all-plugins;
    # jupyter.enable = mkDefault true;  #TODO: move to home-manager config
    # onedrive.enable = mkDefault true;  #TODO: move to home-manager config
    # ollama.enable = mkDefault true;  #TODO: move to home-manager config
    playerctl.enable = true;
    printing.enable = true;
    printing.cups-pdf.enable = true;
    printing.drivers = with pkgs; [
      hplip
    ];
    # protonmail-bridge.enable  #TODO: move to home-manager config
    # samba.enable = true;
    # tabby.enable = true;  #TODO: move to home-manager config
    # tailscale.enable = true;  #TODO: move to home-manager config
    udisks2.enable = true;
  };

  # Security
  security.polkit.enable = true;

  system.stateVersion = "24.11";
}
