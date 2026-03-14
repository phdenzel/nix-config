{pkgs, ...}: {
  services.desktopManager.plasma6.enable = true;

  environment.plasma6.excludePackages = with pkgs.kdePackages; [
    konsole
    oxygen
    khelpcenter
    elisa
    gwenview
    dolphin
    spectacle
    krdp
  ];

  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.kdePackages.xdg-desktop-portal-kde];
  };

  programs.dconf.enable = true;
}
