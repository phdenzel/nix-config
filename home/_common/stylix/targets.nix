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
    ghostty.enable = true;
    gnome.enable = true;
    gtk.enable = true;
    hyprpaper.enable = true;
    kde.enable = true;
    kitty.enable = true;
    mpv.enable = true;
    qt.enable = true;
    starship.enable = false;
    swaync.enable = true;
    tmux.enable = true;
    yazi.enable = true;
    zathura.enable = true;
  };
}
