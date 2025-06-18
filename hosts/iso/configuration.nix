# This is a minimal configuration for initial build onto an iso.
# Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{pkgs, ...}: {
  imports = [
    ../_common/nix.nix # default nix configuration
    ../_common/openssh.nix # openSSH configs
    ../_common/crypt-utils.nix # cryptographic tool collection
    ../_common/cli-utils.nix # cli tool collection
    ../../modules/intl.nix # internationalization configs
  ];

  isoImage.contents = [
    {
      source = ./install-configuration.nix;
      target = "/local/etc/nixos/configuration.nix";
    }
  ];
  system.userActivationScripts.nix-config-repository.text = ''
     ${pkgs.git}/bin/git clone https://github.com/phdenzel/nix-config.git /local/home/nixos/nix-config
  '';

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = pkgs.lib.mkForce ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];
  };

  networking.hostName = "nixos-iso"; # Define your hostname.
  time.timeZone = "Europe/Zurich";
  i18n.defaultLocale = "en_US.UTF-8";

  environment.systemPackages = with pkgs; [
    emacs
  ];

  # Root configuration
  users.users.root = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFyUOUvvJENjM7fNdGW/9ljjJnPEGHlt1pYFYRx2mZIW phdenzel@sol"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOr6HdrDEH1BztobKQo9xZqlqqYUEWTuz5+QricptROm phdenzel@fenrix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZofJltIURsWCGEc+H5wyp4WJ3GGkcjPR5THptcR1dg phdenzel@asahi"
    ];
  };

  # More open OpenSSH settings.
  systemd.services.sshd.wantedBy = pkgs.lib.mkForce ["multi-user.target"];
  services.openssh.settings.PermitRootLogin = pkgs.lib.mkForce "yes";

  # For more information, see `man configuration.nix` or
  # https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion
  system.stateVersion = "25.05"; # Did you read the comment?
}
