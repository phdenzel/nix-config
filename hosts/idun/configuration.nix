# NixOS configuration file for idun
{
  pkgs,
  lib,
  config,
  ...
}:
with lib; {
  imports = [
    ../_common
    ../../modules
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
    hashedPasswordFile = config.sops.secrets."passwd/idun".path;
  };

  # Networking
  networking.hostName = "idun";
  # networking.wireless.enable = true;  # Enable wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  # systemd.services.NetworkManager-wait-online.enable = false;

  # System-wide packages
  environment.defaultPackages = [];
  environment.systemPackages = with pkgs; [
    age
    bat
    eza
    fzf
    gh
    just
    kitty
    rsync
    sddm-astronaut
    sops
    strace
  ];

  # System-wide programs
  programs = {
    dconf.enable = true; # for GNOME
    foot = {
      enable = mkDefault true;
      enableBashIntegration = mkDefault true;
      enableZshIntegration = mkDefault true;
    };
    git.enable = mkDefault true;
    gnupg.agent = {
      enable = mkDefault true;
      enableSSHSupport = mkDefault true;
    };
    less.enable = mkDefault true;
    thunar = {
      enable = mkDefault true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-media-tags-plugin
        thunar-volman
      ];
    };
    tmux.enable = mkDefault true;
    vim.enable = mkDefault true;
    xfconf.enable = mkDefault true; # for thunar
    zsh.enable = mkDefault true;
  };

  # System-wide services
  services = {
    avahi = {
      enable = mkDefault true;
      nssmdns4 = mkDefault true;
      openFirewall = mkDefault true;
    };
    btrfs.autoScrub.enable = mkDefault true;
    emacs.enable = mkDefault true;
    pipewire = {
      enable = mkDefault true;
      # alsa.enable = mkDefault true;
      # alsa.support32Bit = mkDefault true;
      # audio.enable = mkDefault true;
      wireplumber.enable = mkDefault true;
    };
    tumbler.enable = mkDefault true;
  };

  # VM specifics
  services.spice-vdagentd.enable = true;

  # Display manager
  services.displayManager.sddm = {
    enable = true;
    package = pkgs.kdePackages.sddm;
    wayland.enable = true;
    theme = "sddm-astronaut-theme";
    extraPackages = [pkgs.sddm-astronaut];
  };

  # Window manager
  programs.hyprland = {
    enable = mkDefault true;
    xwayland.enable = mkDefault true;
  };
  programs.waybar.enable = mkDefault true;
  programs.hyprlock.enable = mkDefault true;
  services.hypridle.enable = mkDefault true;

  # Desktop environment as backup (if Hyprland is bricked)
  services.xserver.desktopManager.gnome.enable = mkDefault true;
  services.gnome = {
    core-utilities.enable = mkDefault false;
    localsearch.enable = mkDefault false;
    tinysparql.enable = mkDefault false;
    games.enable = mkDefault false;
    core-developer-tools.enable = mkDefault true;
  };
  services.displayManager.sessionPackages = with pkgs; [
    gnome-session.sessions
  ];

  # SSH setup
  services.openssh = {
    enable = mkDefault true;
    # disable RSA keys
    hostKeys = [
      {
        path = "/etc/ssh/ssh_host_ed25519_key";
        rounds = 64;
        type = "ed25519";
      }
    ];
    settings.PermitRootLogin = mkDefault "no";
    allowSFTP = mkDefault true;
  };

  # Security
  security.rtkit.enable = mkDefault true; # recommended for pipewire
  security.polkit.enable = mkDefault true;

  system.stateVersion = "24.11";
}
