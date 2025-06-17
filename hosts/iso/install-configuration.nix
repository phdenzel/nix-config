# This is a minimal configuration for initial install from an iso.
# Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{ config, lib, pkgs, ... }: {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = false;
    grub.enable = true;
    grub.efiSupport = true;
    grub.devices = ["nodev"];
    efi.canTouchEfiVariables = true;
  };
  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.
  time.timeZone = "Europe/Zurich";
  i18n.defaultLocale = "en_US.UTF-8";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.phdenzel = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  #   packages = with pkgs; [
  #     tree
  #   ];
  # };

  environment.systemPackages = with pkgs; [
    age
    bat
    gh
    git
    just
    ssh-to-age
    sops
    tree
    vim
    wget
    yq-go
  ];

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    # change this to "no" or "prohibit-password" afterwards
    settings.PermitRootLogin = "yes";
    hostKeys = [
      {
        path = "/etc/ssh/ssh_host_ed25519_key";
        rounds = 64;
        type = "ed25519";
      }
    ];
  };

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.05"; # Did you read the comment?
}
