# NixOS configuration file for asahi
{
  pkgs,
  config,
  self,
  ...
}: let
  hostName = "asahi";
  hostPlatform = "aarch64-darwin";
in {
  nixpkgs.hostPlatform = hostPlatform;

  imports = [
    ../_common/darwin.nix
  ];

#  boot = {
#    # Kernel
#    kernelPackages = pkgs.linuxPackages_latest;
#    kernelParams = ["mem_sleep_default=deep"];
#    # Bootloader
#    #loader.systemd-boot.enable = true;
#    #loader.systemd-boot.editor = false;
#    loader.efi.canTouchEfiVariables = true;
#    loader.grub.enable = true;
#    loader.grub.efiSupport = true;
#    loader.grub.devices = ["nodev"];
#    supportedFilesystems = ["nfs"];
#    # /tmp as tmpfs
#    tmp = {
#      useTmpfs = false;
#      tmpfsSize = "5%";
#      cleanOnBoot = (!config.boot.tmp.useTmpfs);
#    };
#    # Boot screen
#    # plymouth.enable = mkDefault true;
#  };

#  # Suspend settings
#  powerManagement.enable = true;
#  services.power-profiles-daemon.enable = true;
#  systemd.sleep.extraConfig = ''
#    HibernateDelaySec=30m
#    SuspendState=mem
#  '';
#  services.logind = {
#    settings.Login = {
#      HandleLidSwitch = "suspend-then-hibernate";
#      HandlePowerKey = "hibernate";
#      HandlePowerKeyLongPress = "poweroff";
#    };
#  };

#  # File system configuration
#  services.btrfs.autoScrub = {
#    enable = true;
#    interval = "weekly";
#    fileSystems = ["/"];
#  };

#  # Language customization (see ../../modules)
#  intl.defaultLocale = "en_US";
#  intl.extraLocale = "de_CH";

#  # Root configuration
#  sops-host.enable = true;
#  users.users.root = {
#    hashedPasswordFile = config.sops.secrets."passwd/${hostName}".path;
#    openssh.authorizedKeys.keys = [
#      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
#    ];
#  };

#   # Networking
#   networking = {
#     hostName = "${hostName}";
#     hostId = "42b816d1";
#     wireless.enable = false;  # wireless via wpa_supplicant.
#     networkmanager.enable = true;
#     enableIPv6 = false;
#   };
#   systemd.services.NetworkManager-wait-online.enable = false;

#   # Local networking
#   services.avahi = {
#     enable = true;
#     domainName = "local";
#     nssmdns4 = true;
#     openFirewall = true;
#   };

  # System-wide packages
  environment.systemPackages = with pkgs; [
    vim
  ];

  # System-wide programs
  programs.zsh.enable = true;
#   programs = {
#     firefox.enable = true;
#     thunderbird.enable = true;
#     winbox.enable = true;
#     winbox.package = pkgs.winbox4;
#   };

#   # System-wide services
#   services = {
#     blueman.enable = true;
#     gnome.gnome-keyring.enable = true;
#     gvfs.enable = true;
#     # fprintd.enable = true;
#     fwupd.enable = true;
#     playerctld.enable = true;
#     printing.enable = true;
#     printing.cups-pdf.enable = true;
#     printing.drivers = with pkgs; [
#       hplip
#     ];
#     udisks2.enable = true;
#   };

  security = {
    pam.services.sudo_local.touchIdAuth = true;
  };

  system.configurationRevision = self.rev or self.dirtyRev or null;
  system.stateVersion = 6;
}
