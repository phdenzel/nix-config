{pkgs, ...}: let
  cursorName = "Bibata-Modern-Classic";
  pointerSize = 24;
in {
  wayland.windowManager.hyprland.settings.env = [
    { _args = [ "HYPRCURSOR_THEME" cursorName ]; }
    { _args = [ "HYPRCURSOR_SIZE" (toString pointerSize) ]; }
  ];
  home.pointerCursor = {
    enable = true;
    package = pkgs.bibata-cursors;
    name = cursorName;
    size = pointerSize;
    gtk.enable = true;
    hyprcursor = {
      enable = true;
      size = pointerSize;
    };
    x11.enable = true;
  };
}
