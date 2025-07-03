{
  lib,
  config,
  ...
}:
with config.colorScheme.palette; {
  wayland.windowManager.hyprland = {
    extraConfig = ''
      device {
        name = razer-razer-pro-click-v2-vertical-edition
        sensitivity = 0.1
        scroll_method = on_button_down
      }

      device {
        name = syna800e:00-06cb:ce67-mouse
        sensitivity = 0.8
      }

      device {
        name = syna800e:00-06cb:ce67-touchpad
        sensitivity = 0.8
      }

      device {
        name = tpps/2-elan-trackpoint
        sensitivity = 0.4
      }

      device {
        name = apple-inc.-magic-trackpad
        sensitivity = 0.8
      }
    '';
    settings = {
      general = {
        border_size = 3;
        gaps_out = 12;
        "col.active_border" = lib.mkDefault "rgba(${turquoise}EE) rgba(${indigo}EE) 45deg";
        "col.inactive_border" = lib.mkDefault "rgba(${mantle}AA)";
        resize_on_border = true;
        extend_border_grab_area = 12;
      };

      decoration = {
        rounding = 10;
        blur = {
          enabled = true;
          size = 3;
          ignore_opacity = false;
          popups = true;
        };
        shadow = {
          enabled = true;
          color = lib.mkDefault "rgba(${crust}CC)";
          color_inactive = lib.mkDefault "rgba(${mantle}80)";
          range = 100;
          render_power = 2;
          offset = "0 12";
          scale = 0.97;
        };
      };

      animations = {
        enabled = true;
        bezier = [
          "easeOutCirc, 0.05, 0.9, 0.1, 1.05"
        ];
        animation = [
          "border, 1, 6, default"
          "borderangle, 1, 4, default"
          "fade, 1, 4, default"
          "windows, 1, 4, easeOutCirc"
          "windowsOut, 1, 4, default, popin 80%"
          "workspaces, 1, 4, default, slide"
        ];
      };

      input = {
        kb_layout = "us";
        kb_options = "ctrl:nocaps, compose:ralt";
        repeat_rate = 64;
        repeat_delay = 180;
        accel_profile = "flat";
        scroll_method = "2fg";
        follow_mouse = 2; # click will move focus, hovering doesn't
        touchpad = {
          disable_while_typing = false;
          clickfinger_behavior = true;
          tap-to-click = false;
        };
        tablet = {
          output = "current";
        };
      };

      gestures = {
        workspace_swipe = true;
        workspace_swipe_fingers = 4;
        workspace_swipe_distance = 500;
        workspace_swipe_invert = false;
        workspace_swipe_min_speed_to_force = 5;
        workspace_swipe_forever = true;
      };

      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
        vrr = 1;
        disable_autoreload = true;
        allow_session_lock_restore = true;
        new_window_takes_over_fullscreen = 2;
      };

      dwindle = {
        pseudotile = true; # master switch for pseudotiling
        preserve_split = true; # you probably want this
      };

      xwayland.force_zero_scaling = true;
      ecosystem = {
        no_update_news = true;
        no_donation_nag = true;
      };
      #debug.disable_logs = true;
    };
  };
}
