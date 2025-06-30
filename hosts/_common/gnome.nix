{pkgs, ...}: {
  services.desktopManager.gnome.enable = true;
  services.gnome = {
    core-apps.enable = false;
    localsearch.enable = false;
    tinysparql.enable = false;
    games.enable = false;
    core-developer-tools.enable = false;
  };
  environment.gnome.excludePackages = with pkgs; [gnome-tour];

  xdg.portal.enable = true;

  programs = {
    dconf.enable = true;
    file-roller.enable = true;
  };
}
