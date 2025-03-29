{...}: {
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "uwsm finalize"
      "pypr --debug /tmp/pypr.log"
      # "hyprlock"
      "waybar"
      "udiskie"
      "lact daemon"      
      # "licht -d"
    ];
  };
}
