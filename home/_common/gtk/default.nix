{ pkgs, lib, ... }: {
  gtk = {
    enable = true;
    theme = {
      name = lib.mkForce "Breeze-Dark";
      package = lib.mkForce pkgs.kdePackages.breeze-gtk;
    };
    iconTheme = {
      name = "breeze-dark";
      package = pkgs.kdePackages.breeze-icons;
    };
  };

  # Needed for GTK4 apps (uses libadwaita color scheme)
  dconf.settings."org/gnome/desktop/interface" = {
    color-scheme = "prefer-dark";
  };
}
