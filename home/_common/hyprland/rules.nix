{...}: {
  wayland.windowManager.hyprland.settings = {
    window_rule = [
      {
        match = { class = "pinentry-.*"; };
        stay_focused = true;
      }
      # Idle inhibitors
      {
        match = { fullscreen = true; };
        idle_inhibit = "focus";
      }
      {
        match = { class = "(mpv|.+exe)$"; };
        idle_inhibit = "focus";
      }
      # Firefox PiP floating window
      {
        match = { title = "^Picture-in-Picture$"; };
        float = true;
        pin = true;
      }
      # Floats
      {
        match = { title = "^File Operation Progress$"; };
        float = true;
      }
      # Scratchpads
      {
        match = { title = "^scratchpad.*$"; };
        float = true;
      }
      {
        match = { class = "^org.pulseaudio.pavucontrol$"; };
        float = true;
      }
    ];
    layer_rule = [
      {
        match = { namespace = "wlogout"; };
        blur = true;
      }
    ];
  };
}
