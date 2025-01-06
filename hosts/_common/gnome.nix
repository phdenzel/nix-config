{pkgs, ...}: {
  services.xserver.desktopManager.gnome.enable = true;
  services.gnome = {
    core-utilities.enable = false;
    localsearch.enable = false;
    tinysparql.enable = false;
    games.enable = false;
    core-developer-tools.enable = false;
  };
  environment.gnome.excludePackages = with pkgs; [gnome-tour];

  programs.dconf.enable = true;
}
