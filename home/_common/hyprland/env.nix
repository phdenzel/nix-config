{...}: {
  wayland.windowManager.hyprland.settings = {
    env = [
      "GDK_BACKEND,wayland,x11"
      "GDK_SCALE,2"
      "MOZ_ENABLE_WAYLAND,1"
      # "QT_QPA_PLATFORMTHEME,kde"
    ];
  };
}
