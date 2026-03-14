{pkgs, ...}: {
  services.desktopManager.plasma6.enable = true;

  environment.plasma6.excludePackages = with pkgs.kdePackages; [
    konsole      # rather use ghostty
    oxygen       # no legacy theme pack
    kate         # rather use emacs, vim, etc.
    khelpcenter  # unnecessary
    elisa        # rather use gapless
    gwenview     # rather use imv
    dolphin      # rather use thunar
    krdp         # no remote desktop
  ];

  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.kdePackages.xdg-desktop-portal-kde];
  };

  programs.dconf.enable = true;
}
