# NixOS configuration file for phinix
{
  pkgs,
  lib,
  ...
}:
with lib; {
  imports = [
    ./disk-config.nix
    ./hardware-configuration.nix
    ../_common
    ../../modules
  ];

  boot = {
    # Kernel
    kernelPackages = pkgs.linuxPackages;
    # kernelPackages = pkgs.linuxPackages_latest;
    # kernelPackages = pkgs.linuxPackages_zen;

    # Bootloader
    loader.grub = {
      enable = true;
      useOSProber = true;
    };

    # /tmp as tmpfs
    tmp = {
      useTmpfs = true;
      tmpfsSize = "5%";
    };

    # Boot screen
    plymouth.enable = mkDefault true;
  };

  # Custom modules (see ../../modules)
  drivers.amdgpu.enable = false;
  drivers.amdgpu.utils.install = false;
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # # User settings
  home-manager.users.phdenzel =
    import ../../home/phdenzel/${config.networking.hostName}.nix;

  # Networking
  networking.hostName = "idun";
  # networking.wireless.enable = true;  # Enable wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  # systemd.services.NetworkManager-wait-online.enable = false;

  # System-wide packages
  environment.defaultPackages = [];
  environment.systemPackages = with pkgs; [
    bat
    eza
    fzf
    gh
    kdePackages.sddm-astronaut
    rsync
    strace
  ];

  # System-wide programs
  programs = {
    foot.enable = mkDefault true;
    foot.enableBashIntegration = mkDefault true;
    foot.enableZshIntegration = mkDefault true;
    git.enable = mkDefault true;
    gnupg.agent = {
      enable = mkDefault true;
      enableSSHSupport = mkDefault true;
    };
    less.enable = mkDefault true;
    thunar.enable = mkDefault true;
    thunar.plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-media-tags-plugin
      thunar-volman
    ];
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

  services.displayManager.enable = mkDefault true;
  services.displayManager.sddm = {
    enable = mkDefault true;
    package = pkgs.kdePackages.sddm;
    wayland.enable = mkDefault true;
    extraPackages = with pkgs; mkDefault [sddm-astronaut];
    theme = mkDefault "sddm-astronaut-theme";
  };

  # Window manager
  programs.hyperland = {
    enable = mkDefault true;
    xwayland.enable = mkDefault true;
  };
  programs.waybar.enable = mkDefault true;
  programs.hyprlock.enable = mkDefault true;
  services.hypridle.enable = mkDefault true;

  # # Desktop environment
  # services.xserver.desktopManager.gnome.enable = mkDefault true;
  # services.gnome = {
  #   core-utilities.enable = mkDefault false;
  #   localsearch.enable = mkDefault false;
  #   tinysparql.enable = mkDefault false;
  #   games.enable = mkDefault false;
  #   core-developer-tools.enable = mkDefault true;
  # };

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
