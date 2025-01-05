# This is a minimal configuration for initial install from an iso.
# Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{pkgs, ...}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.
  time.timeZone = "Europe/Zurich";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
  };

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
    sops
    tree
    vim
    wget
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

  # For more information, see `man configuration.nix` or
  # https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion
  system.stateVersion = "24.11"; # Did you read the comment?
}
