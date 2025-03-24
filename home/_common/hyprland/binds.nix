{...}: let
  uwsmRun = program: "uwsm app -- ${program}";
  uwsmTerm = program: "uwsm app -T -- ${program}";
  uwsmToggle = program: "pkill ${program} || uwsm app -- ${program}";
  uwsmOnce = program: "pgrep ${program} || uwsm app -- ${program}";
in {
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    
    "$term" = "ghostty";
    "$files" = "thunar";
    "$filesAlt" = "yazi";
    "$editor" = "emacsclient -c --alternate-editor=\"emacs\"";
    "$editorAlt" = "vim";
    "$browser" = "firefox";
    "$browserAlt" = "qutebrowser";
    "$email" = "thunderbird";
    "$emailAlt" = "$editor -e '(himalaya)'";
    "$passwm" = "pass";

    bind = [
      # App launcher
      "$mod, Slash, exec, rofi -show drun -run-command \"${uwsmRun "{cmd}"}\""
      "$mod, P, exec, rofi-pass -b wl-clipboard"

      # Scratchpad
      "$mod SHIFT, Return, exec, pypr toggle term"
      "$mod SHIFT, D, exec, pypr toggle filemgr"
      "$mod SHIFT, Space, exec, pypr toggle filemgr"
      "$mod SHIFT, D, exec, pypr toggle filemgr"
      "$mod, W, exec, pypr toggle clipboard"

      # Status bar
      "$mod, B, exec, killall -SIGUSR1 waybar" # hide bar (w/o killing it)
      "$mod SHIFT, B, exec, killall -SIGUSR2 waybar" # reload bar (no restart)
      "$mod ALT, B, exec, killall waybar; waybar" # restart bar

      # Essential apps
      "$mod, Return, exec, ${uwsmRun "$term"}"
      "$mod, E, exec, $editor"
      "$mod, D, exec, ${uwsmRun "$files"}"
      "$mod ALT, D, exec, ${uwsmTerm "$filesAlt"}"

      # DPMS
      "$mod, Escape, exec, ${uwsmOnce "hyprlock"}"
      "$mod CTRL, Escape, exec, ${uwsmToggle "wlogout"} -p layer-shell"

      # Screenshots
      "$mod CTRL, minus, exec, ${uwsmOnce "flameshot"} full"
      "$mod CTRL, equal, exec, ${uwsmOnce "flameshot"} gui"
      "$mod ALT, minus, exec, ${uwsmOnce "flameshot"} screen"
      "$mod ALT, equal, exec, ${uwsmOnce "flameshot"} gui -c"

      # Window navigation
      "$mod, Tab, cyclenext"
      "$mod, H, movefocus, l"
      "$mod, J, movefocus, d"
      "$mod, K, movefocus, u"
      "$mod, L, movefocus, r"
      "$mod, left, movefocus, l"
      "$mod, down, movefocus, d"
      "$mod, up, movefocus, u"
      "$mod, right, movefocus, r"

      # Window movement
      "$mod SHIFT, H, movewindow, l"
      "$mod SHIFT, J, movewindow, d"
      "$mod SHIFT, K, movewindow, u"
      "$mod SHIFT, L, movewindow, r"

      # Window control
      "$mod ALT, H, movetoworkspace, -1"
      "$mod ALT, L, movetoworkspace, +1"
      "$mod SHIFT, C, killactive"
      "$mod, F, fullscreen, 0"
      "$mod, T, togglefloating, active"
      "$mod, mouse:274, killactive"

      # Workspace navigation
      "$mod CTRL, H, workspace, -1"
      "$mod CTRL, L, workspace, +1"
      "$mod, 1, workspace, 1"
      "$mod, 2, workspace, 2"
      "$mod, 3, workspace, 3"
      "$mod, 4, workspace, 4"
      "$mod, 5, workspace, 5"
      "$mod, 6, workspace, 6"
      "$mod, 7, workspace, 7"
      "$mod, 8, workspace, 8"
      "$mod, 9, workspace, 9"
      "$mod, 0, workspace, 10"

      # Cross-workspace navigation of windows
      "$mod SHIFT, 1, movetoworkspace, 1"
      "$mod SHIFT, 2, movetoworkspace, 2"
      "$mod SHIFT, 3, movetoworkspace, 3"
      "$mod SHIFT, 4, movetoworkspace, 4"
      "$mod SHIFT, 5, movetoworkspace, 5"
      "$mod SHIFT, 6, movetoworkspace, 6"
      "$mod SHIFT, 7, movetoworkspace, 7"
      "$mod SHIFT, 8, movetoworkspace, 8"
      "$mod SHIFT, 9, movetoworkspace, 9"
      "$mod SHIFT, 0, movetoworkspace, 10"

      # Cross-workspace navigation of workspaces & windows
      "$mod SHIFT CTRL, 1, movetoworkspacesilent, 1"
      "$mod SHIFT CTRL, 2, movetoworkspacesilent, 2"
      "$mod SHIFT CTRL, 3, movetoworkspacesilent, 3"
      "$mod SHIFT CTRL, 4, movetoworkspacesilent, 4"
      "$mod SHIFT CTRL, 5, movetoworkspacesilent, 5"
      "$mod SHIFT CTRL, 6, movetoworkspacesilent, 6"
      "$mod SHIFT CTRL, 7, movetoworkspacesilent, 7"
      "$mod SHIFT CTRL, 8, movetoworkspacesilent, 8"
      "$mod SHIFT CTRL, 9, movetoworkspacesilent, 9"
      "$mod SHIFT CTRL, 0, movetoworkspacesilent, 10"

      # Monitor navigation
      "$mod, comma, focusmonitor, 0"
      "$mod, period, focusmonitor, 1"
      "$mod CTRL, comma, movecurrentworkspacetomonitor, 0"
      "$mod CTRL, period, movecurrentworkspacetomonitor, 1"
      "$mod SHIFT, comma, swapactiveworkspaces, 0 1"
      "$mod SHIFT, period, swapactiveworkspaces, 1 0"
    ];

    bindm = [
      # Window control
      "$mod, mouse:272, movewindow"
      "$mod SHIFT, mouse:272, resizewindow"
    ];

    bindl = [
      # Media
      ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
      ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
      ", XF86AudioPlay, exec, playerctl play-pause"
      ", XF86AudioPrev, exec, playerctl position -5"
      ", XF86AudioNext, exec, playerctl position +5"
    ];

    bindo = [
      ", XF86AudioPrev, exec, playerctl previous"
      ", XF86AudioNext, exec, playerctl next"
      
    ];

    bindle = [
      ", XF86AudioRaiseVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 1%+"
      ", XF86AudioLowerVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 1%-"
      ", XF86MonBrightnessUp, exec, "
      ", XF86MonBrightnessDown, exec, "
    ];
  };
}
