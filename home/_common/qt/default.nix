{ pkgs, ... }: {
  qt = {
    enable = true;
    platformTheme.name = "kde";   # Use KDE platform plugin (reads kdeglobals)
    style = {
      name = "breeze";            # KDE's Breeze widget style
      package = pkgs.kdePackages.breeze;
    };
  };
}
