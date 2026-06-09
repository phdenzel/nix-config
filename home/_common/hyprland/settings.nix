{
  lib,
  config,
  ...
}:
with config.colorScheme.palette; {
  wayland.windowManager.hyprland.settings = {
    config = {
      general = {
        border_size = 3;
        gaps_out = 12;
        "col.active_border" = lib.mkDefault "rgba(${turquoise}EE) rgba(${indigo}EE) 45deg";
        "col.inactive_border" = lib.mkDefault "rgba(${mantle}AA)";
        resize_on_border = true;
        extend_border_grab_area = 12;
        layout = "dwindle";
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
          tap_to_click = false;
        };
        tablet = {
          output = "current";
        };
      };

      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
        vrr = 1;
        disable_autoreload = true;
        allow_session_lock_restore = true;
        force_default_wallpaper = 0;
      };

      dwindle = {
        preserve_split = true;
      };

      scrolling = {
        explicit_column_widths = "0.333, 0.5, 1.0";
      };

      xwayland.force_zero_scaling = true;
      ecosystem = {
        no_update_news = true;
        no_donation_nag = true;
      };

      gestures = {
        workspace_swipe_distance = 500;
        workspace_swipe_invert = false;
        workspace_swipe_min_speed_to_force = 5;
        workspace_swipe_forever = true;
        workspace_swipe_use_r = true;
      };
      #debug.disable_logs = true;
    };

    device = [
      {
        name = "razer-razer-pro-click-v2-vertical-edition";
        sensitivity = 0.1;
        scroll_method = "on_button_down";
      }
      {
        name = "syna800e:00-06cb:ce67-mouse";
        sensitivity = 0.8;
      }
      {
        name = "syna800e:00-06cb:ce67-touchpad";
        sensitivity = 0.8;
      }
      {
        name = "tpps/2-elan-trackpoint";
        sensitivity = 0.4;
      }
      {
        name = "apple-inc.-magic-trackpad";
        sensitivity = 0.8;
      }
    ];

    gesture = [
      {
        fingers = 3;
        direction = "horizontal";
        action = "workspace";
      }
      {
        fingers = 4;
        direction = "horizontal";
        action = "workspace";
      }
      {
        fingers = 3;
        direction = "down";
        action = "close";
      }
      {
        fingers = 4;
        direction = "down";
        action = "close";
      }
      {
        fingers = 3;
        direction = "up";
        scale = 1.5;
        action = "fullscreen";
      }
      {
        fingers = 4;
        direction = "up";
        scale = 1.5;
        action = "fullscreen";
      }
      {
        fingers = 3;
        direction = "pinchin";
        scale = 1.5;
        action = "move";
      }
      {
        fingers = 4;
        direction = "pinchin";
        scale = 1.5;
        action = "move";
      }
      {
        fingers = 3;
        direction = "pinchout";
        scale = 1.5;
        action = "float";
      }
      {
        fingers = 4;
        direction = "pinchout";
        scale = 1.5;
        action = "float";
      }
    ];
  };
}
