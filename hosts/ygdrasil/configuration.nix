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
    ../_common/security.nix # security configs
    ../_common/openssh.nix # openSSH configs
    ../_common/crypt-utils.nix # cryptographic tool collection
    ../_common/emacs.nix # editor and god tool
    ../_common/cli-utils.nix # cli tool collection
    ../_common/dev-utils.nix # dev tool collection
    ../_common/computing.nix # computing tool collection
    ../_common/ollama.nix # local LLM services
    ../_common/texlive.nix # full TeXLive package
    ../_common/tx-rx.nix # transmission / reception
    # ../_srv/dashboards.nix # service dashboards (homepage-dashboard, glance, ...)
    # ../_srv/admin.nix # monitoring (cockpit, uptime-kuma, gotify, ...)
    # ../_srv/proxy.nix # server proxy services (traefik, crowdsec, keycloak, ...)
    # ../_srv/dns.nix # DNS/Ad-blocking service (blocky, ...)
    # ../_srv/vpn.nix # VPN services (wireguard, tailscale, ...)
    # ../_srv/forgejo.nix # git forge service (forgejo)
    # # ../_srv/cloud.nix # cloud service (filebrowser, opencloud, ...)
    # ../_srv/immich.nix # image hosting service
    # ../_srv/jellyfin.nix # media streaming service
    # ../_srv/servarr.nix # servarr stack
    # ../_srv/home-assistant.nix # home assistant service
    # ../_srv/db.nix # database services (grafana, influxdb, ...)
    # ../_srv/vikunja.nix # ToDo management service
    # ../_srv/mealie.nix # recipe service
    # ../_srv/ghostfolio.nix # wealth management service
    ../../modules # AMD/Nvidia, Internationalization configs
    inputs.hardware.nixosModules.common-cpu-amd
    inputs.hardware.nixosModules.common-cpu-amd-pstate
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
  hardware.cpu.amd.updateMicrocode = true;

  # Language customization (see ../../modules)
  intl.defaultLocale = "en_US";
  intl.extraLocale = "de_CH";

  # Root configuration
  sops-host.enable = true;
  users.users.root = {
    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
  };

  # Networking
  networking = {
    hostName = "${hostName}";
    # hostId = "";
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
    lm_sensors
    udiskie
    usbutils
    # 
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

  system.stateVersion = "24.11";
}
