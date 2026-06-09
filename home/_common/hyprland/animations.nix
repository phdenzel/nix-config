{...}: {
  wayland.windowManager.hyprland.settings = {
    curve = [
      { _args = [ "easeOutCirc" { type = "bezier"; points = [[0.05 0.9] [0.1 1.05]]; } ]; }
    ];
    animation = [
      { leaf = "border";      enabled = true; speed = 6; bezier = "default";     }
      { leaf = "borderangle"; enabled = true; speed = 4; bezier = "default";     }
      { leaf = "fade";        enabled = true; speed = 4; bezier = "default";     }
      { leaf = "windows";     enabled = true; speed = 4; bezier = "easeOutCirc"; }
      { leaf = "windowsOut";  enabled = true; speed = 4; bezier = "default"; style = "popin 80%"; }
      { leaf = "workspaces";  enabled = true; speed = 4; bezier = "default"; style = "slide";     }
    ];
  };
}
