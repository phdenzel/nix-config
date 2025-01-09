{pkgs, lib, ...}: with lib; {
  environment.systemPackages = with pkgs; [
    emacsclient-commands
  ];
  # technically not a CLI tool
  services.emacs = {
    enable = mkDefault true;
    defaultEditor = mkDefault true;
  };
}
