{...}: {
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "pypr --debug /tmp/pypr.log"
      # "hyprlock"
      "udiskie &"
      # "licht -d"
    ];
  };
}
