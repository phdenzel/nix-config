# NixOS configuration file for ygdrasil
{
  pkgs,
  config,
  inputs,
  ...
}: let
  hostName = "ygdrasil";
in {
  imports = [
    ../_common # default nix (and sops-nix) configuration
    ../_common/kde.nix # desktop for direct graphical access
    ../_common/fonts.nix # font packages
    ../_common/thunar.nix # file manager
    ../_common/security.nix # security configs
    ../_common/openssh.nix # openSSH configs
    ../_common/crypt-utils.nix # cryptographic tool collection
    ../_common/emacs.nix # editor and god tool
    ../_common/cli-utils.nix # cli tool collection
    ../_common/dev-utils.nix # dev tool collection
    # ../_srv/computing.nix # computing tool collection
    # ../_srv/ollama.nix # local LLM services
    ../_srv/dashboards.nix # server dashboards (homepage-dashboard, glances, ...)
    # ../_srv/admin.nix # monitoring (cockpit, uptime-kuma, gotify, ...)
    ../_srv/traefik # server proxy services
    ../_srv/traefik/ygdrasil.nix
    # ../srv/crowdsec.nix # security services
    # ../srv/keycloak.nix # security services 
    # ../_srv/vpn.nix # VPN services (wireguard, tailscale, ...)
    ../_srv/forgejo.nix # git forge service (forgejo)
    ../_srv/opencloud.nix # cloud service
    # ../_srv/immich.nix # image hosting service
    ../_srv/jellyfin.nix # media streaming service
    # ../_srv/servarr.nix # servarr stack
    ../_srv/transmission.nix
    # ../_srv/home-assistant.nix # home assistant service
    ../_srv/vikunja.nix # ToDo management service
    ../_srv/mealie.nix # recipe service
    ../../modules # Internationalization configs
    inputs.hardware.nixosModules.common-cpu-amd-zenpower
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
    supportedFilesystems = ["btrfs" "nfs"];
    initrd.supportedFilesystems = ["btrfs"];
    # /tmp as tmpfs
    tmp = {
      useTmpfs = true;
      tmpfsSize = "25%";
    };
    # Boot screen
    # plymouth.enable = mkDefault true;
  };

  # File system configuration
  services.btrfs.autoScrub = {
    enable = true;
    interval = "weekly";
    fileSystems = ["/" "/data"];
  };
  # Store/media directories (set permissions, create if missing)
  users.groups.media = {};
  
  systemd.tmpfiles.rules = [
    "d  /data/media               02775  root      media         -  -"
    "d  /data/media/Videos        02770  phdenzel  media         -  -"
    "d  /data/media/Books         02770  phdenzel  media         -  -"
    "d  /data/media/Music         02770  phdenzel  media         -  -"
    "d  /data/media/Pictures      02770  phdenzel  media         -  -"
    "d  /data/store/transmission  02770  phdenzel  transmission  -  -"
    "d  /data/store/forgejo       02770  phdenzel  forgejo       -  -"
    "d  /data/store/opencloud     02775  phdenzel  opencloud     -  -"
    # "d  /data/store/immich        02775  phdenzel  immich        -  -"
  ];
  system.activationScripts.mediaPermissions = ''
    for dir in Videos Books Music; do
      find /data/media/$dir -type d -exec chmod 02770 {} +
      find /data/media/$dir -type f -exec chmod 0660 {} +
      chown -R phdenzel:media /data/media/$dir
    done;
    # for dir in Pictures; do
    #   find /data/media/$dir -type d -exec chmod 02770 {} +
    #   find /data/media/$dir -type f -exec chmod 0660 {} +
    #   chown -R phdenzel:media /data/media/$dir
    # done;
  '';

  # Hardware customization
  # nixpkgs.config.cudaSupport = true;
  # services.ollama.acceleration = "cuda";

  # Language customization (see ../../modules)
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  sops-host.enable = true;
  users.users.root = {
    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
    # SSH root access (for now)
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
    # hostId = "";
    # wireless.enable = true;  # wireless via wpa_supplicant.
    networkmanager.enable = true;
    enableIPv6 = false;
  };
  systemd.network.wait-online.enable = false;
  boot.initrd.systemd.network.wait-online.enable = false;

  # Local networking
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
    };
  };

  # System-wide packages
  environment.defaultPackages = [];
  environment.systemPackages = with pkgs; [
    lm_sensors
    udiskie
    usbutils
    borgbackup
    cyrus-sasl-xoauth2
    iputils
    pizauth
    wget
  ];

  # System-wide programs
  programs = {
    winbox.enable = true;
    winbox.package = pkgs.winbox4;
  };

  # System-wide services
  services = {
    blueman.enable = true;
    gnome.gnome-keyring.enable = true;
    gvfs.enable = true;
    fwupd.enable = true;
    udisks2.enable = true;
  };

  system.stateVersion = "25.05";
}
