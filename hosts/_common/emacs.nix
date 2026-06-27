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
  services.emacs =
    if pkgs.stdenv.isDarwin
    then {
      # nix-darwin only defines: enable, package, additionalPath, exec
      enable = mkDefault true;
      package = mkDefault pkgs.emacs-macport;
    }
    else {
      install = mkDefault true;
      package = mkDefault pkgs.emacs-pgtk;
      startWithGraphical = true;
      defaultEditor = mkDefault true;
    };
}
