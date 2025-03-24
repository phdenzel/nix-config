{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    emacsclient-commands
  ];
  # technically not a CLI tool
  services.emacs = {
    install = mkDefault true;
    package = mkDefault pkgs.emacs-pgtk;
    startWithGraphical = true;
    defaultEditor = mkDefault true;
  };
}
