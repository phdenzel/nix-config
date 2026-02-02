{...}: {
  wayland.windowManager.hyprland.settings = {
    windowrule = [
      "stay_focused on, match:class (pinentry-)(.*)"
      # Idle inhibitors
      "idle_inhibit focus, match:fullscreen true"
      "idle_inhibit focus, match:class (mpv|.+exe)$"
      # Firefox's PiP floating window
      "float on, match:title ^(Picture-in-Picture)$"
      "pin on, match:title ^(Picture-in-Picture)$"

      # Floats
      "float on, match:title ^(File Operation Progress)$"

      # Scratchpads
      "float on, match:title ^(scratchpad.*)$"
      "float on, match:class ^(org.pulseaudio.pavucontrol)$"
    ];
    layerrule = [
      "blur on, match:namespace wlogout"
    ];
  };
}
