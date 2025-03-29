{...}: {
  wayland.windowManager.hyprland.settings = {
    windowrulev2 = [
      # Idle inhibitors
      "idleinhibit fullscreen, fullscreen:1"
      "idleinhibit focus, class:(mpv|.+exe)$"

      # Firefox's PiP floating window
      "float, title:^(Picture-in-Picture)$"
      "pin, title:^(Picture-in-Picture)$"

      # Scratchpads
      "float, title:^(scratchpad.*)$"
      "float, class:^(org.pulseaudio.pavucontrol)$"
    ];
  };
}
