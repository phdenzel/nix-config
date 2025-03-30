{config, ...}: {
  stylix.targets = {
    alacritty.enable = true;
    bat.enable = true;
    btop.enable = true;
    emacs.enable = false;
    firefox = {
      enable = true;
      firefoxGnomeTheme.enable = true;
      profileNames = [ "${config.home.username}" ];
    };
    foot.enable = true;
    ghostty.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    hyprpaper.enable = true;
    kitty.enable = true;
  };
}
