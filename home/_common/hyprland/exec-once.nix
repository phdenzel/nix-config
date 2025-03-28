{...}: {
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "uwsm finalize"
      "pypr --debug /tmp/pypr.log"
      # "hyprlock"
      "udiskie"
      "lact daemon"
      # "licht -d"
    ];
  };
}
