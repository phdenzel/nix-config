# NixOS configuration file for asahi
{
  pkgs,
  config,
  self,
  ...
}: let
  primaryUser = "phdenzel";
  hostName = "asahi";
  hostPlatform = "aarch64-darwin";
in {
  nixpkgs.hostPlatform = hostPlatform;

  imports = [
    ../_common/darwin.nix
    ../_common/fonts.nix # font packages
    ../_common/emacs.nix # editor and god tool
    ../_common/ai.nix # miscellaneous AI tools
    ../_common/cli-utils.nix # cli tool collection
    ../_common/crypt-utils.nix # cryptographic tool collection
    ../_common/containerization.nix # podman and oci utils
    ../_common/comm.nix # communication apps
    ../_common/dev-utils.nix # dev tool collection
    ../_common/openssh.nix # openSSH configs
    ../_common/jupyterlab-darwin.nix # jupyterlab env + launchd daemon
    ../_common/texlive.nix # full TeXLive package
    ../_common/homebrew.nix # darwin-only packages not found in nixpkgs
  ];

  power = {
    sleep = {
      allowSleepByPowerButton = true;
      computer = 10; # minutes
      display = 15; # minutes
      harddisk = 10; # minutes
    };
  };

  # Root configuration
  sops-host.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLoBbnz9XBvuq7QIUT1cPpyn32PWJFEnH1tPJAidJvO phdenzel@phinix"
  ];


  # Networking
  networking = {
    computerName = "${hostName}";
    hostName = "${hostName}";
    localHostName = "${hostName}";
    wakeOnLan.enable = true;
  };

  # System-wide packages
  environment.systemPackages = with pkgs; [
    easytag
    exiftool
    firefox
    ghostty-bin
    gimp2
    # handbrake # broken
    imagemagick
    inkscape
    mkvtoolnix
    pinentry_mac
    pizauth
    syncthing-macos
    vim
    vlc-bin
    winbox4
    wget
  ];

  # System-wide programs
  # programs = {
  # };

  # System-wide services
  # services = {
  # };

  # The Emacs daemon is provided by home-manager (running the configured
  # programs.emacs.finalPackage), so disable the plain system-level daemon
  # from hosts/_common/emacs.nix to avoid a second, unconfigured daemon.
  services.emacs.enable = false;

  security = {
    pam.services.sudo_local.touchIdAuth = true;
  };

  system.primaryUser = primaryUser;
  system.configurationRevision = self.rev or self.dirtyRev or null;
  system.stateVersion = 6;
}
