{...}: {
  wayland.windowManager.hyprland.settings = {
    env = [
      { _args = [ "GDK_BACKEND" "wayland,x11,*" ]; }
      { _args = [ "GDK_SCALE" "2" ]; }
      { _args = [ "MOZ_ENABLE_WAYLAND" "1" ]; }
      # { _args = [ "QT_QPA_PLATFORMTHEME" "kde" ]; }
    ];
  };
}
