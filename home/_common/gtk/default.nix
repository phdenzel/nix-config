{ pkgs, lib, ... }: {
  gtk = {
    enable = true;
    colorScheme = "dark";
    theme = {
      name = lib.mkDefault "Breeze Dark";
      package = lib.mkDefault pkgs.kdePackages.breeze-gtk;
    };
    iconTheme = {
      name = lib.mkDefault "Breeze-Dark";
      package = lib.mkDefault pkgs.kdePackages.breeze-icons;
    };
  };

  # Needed for GTK4 apps (uses libadwaita color scheme)
  dconf.settings."org/gnome/desktop/interface" = {
    color-scheme = "prefer-dark";
  };
}
