{...}: {
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "uwsm finalize"
      "pypr --debug /tmp/pypr.log"
      "waybar"
      "hyprsunset"
      "udiskie"
      "lact daemon"
      # "licht -d"
    ];
  };
}
