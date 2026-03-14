{
  pkgs,
  lib,
  ...
}: {
  qt = {
    enable = true;
    platformTheme.name = "kde";   # Use KDE platform plugin (reads kdeglobals)
    # style = {
    #   name = lib.mkForce "breeze";  # KDE's Breeze widget style
    #   package = lib.mkForce pkgs.kdePackages.breeze;
    # };
  };
}
