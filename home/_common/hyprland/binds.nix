{lib, ...}: let
  uwsmRun    = program: "uwsm app -- ${program}";
  uwsmTerm   = program: "uwsm app -T -- ${program}";
  uwsmOnce   = program: "pgrep ${program} || uwsm app -- ${program}";
  term       = "ghostty";
  files      = "thunar";
  filesAlt   = "yazi";
  editor     = ''emacsclient -c --alternate-editor="emacs"'';
  # Helper: plain bind (key + dispatcher)
  mk = key: dsp: {
    _args = [
      (lib.generators.mkLuaInline key)
      (lib.generators.mkLuaInline dsp)
    ];
  };
  # Helper: bind with options table (locked, repeating, mouse, once, ...)
  mkOpts = key: dsp: opts: {
    _args = [
      (lib.generators.mkLuaInline key)
      (lib.generators.mkLuaInline dsp)
      opts
    ];
  };
  # Helper: exec bind — wraps cmd in hl.dsp.exec_cmd(); escapes double quotes
  mkExec     = key: cmd:       mk     key ''hl.dsp.exec_cmd("${lib.escape ["\""] cmd}")'';
  mkExecOpts = key: cmd: opts: mkOpts key ''hl.dsp.exec_cmd("${lib.escape ["\""] cmd}")'' opts;
in {
  wayland.windowManager.hyprland.settings = {
    mod = {
      _var = "SUPER";
    };
    _thirds_active = {
      _var = lib.generators.mkLuaInline "false";
    };

    bind = [
      # App launcher
      (mkExec ''mod .. " + Slash"'' ''rofi -show drun -run-command 'uwsm app -- {cmd}' -calc-command "echo -n '{result}' | wl-copy"'')
      (mkExec ''mod .. " + P"''     "rofi-pass -b wl-clipboard")

      # Scratchpad
      (mkExec ''mod .. " + SHIFT + Return"'' "pypr toggle term")
      (mkExec ''mod .. " + SHIFT + Space"''  "pypr toggle filemgr")
      (mkExec ''mod .. " + SHIFT + W"''      "pypr toggle clipboard")

      # Status bar
      (mkExec ''mod .. " + B"''         "pkill --signal SIGUSR1 waybar")
      (mkExec ''mod .. " + SHIFT + B"'' "pkill --signal SIGUSR2 waybar")
      (mkExec ''mod .. " + ALT + B"''   "pkill waybar; waybar")

      # Essential apps
      (mkExec ''mod .. " + Return"''   (uwsmRun term))
      (mkExec ''mod .. " + E"''        editor)
      (mkExec ''mod .. " + N"''        (uwsmRun "swaync-client -t -sw"))
      (mkExec ''mod .. " + D"''        (uwsmRun files))
      (mkExec ''mod .. " + ALT + D"''  (uwsmTerm filesAlt))

      # DPMS / session
      (mkExec ''mod .. " + Escape"''        (uwsmOnce "hyprlock"))
      (mkExec ''mod .. " + CTRL + Escape"'' "${uwsmRun "wlogout"} -p layer-shell -b 5")

      # Screenshots (TODO: currently not working)
      (mkExec ''mod .. " + CTRL + minus"'' "${uwsmOnce "flameshot"} full")
      (mkExec ''mod .. " + CTRL + equal"'' "${uwsmOnce "flameshot"} gui")
      (mkExec ''mod .. " + ALT + minus"''  "${uwsmOnce "flameshot"} screen")
      (mkExec ''mod .. " + ALT + equal"''  "${uwsmOnce "flameshot"} gui -c")

      # Window navigation
      (mk ''mod .. " + Tab"''   "hl.dsp.window.cycle_next()")
      (mk ''mod .. " + H"''     ''hl.dsp.focus({ direction = "left" })'')
      (mk ''mod .. " + J"''     ''hl.dsp.focus({ direction = "down" })'')
      (mk ''mod .. " + K"''     ''hl.dsp.focus({ direction = "up" })'')
      (mk ''mod .. " + L"''     ''hl.dsp.focus({ direction = "right" })'')
      (mk ''mod .. " + left"''  ''hl.dsp.focus({ direction = "left" })'')
      (mk ''mod .. " + down"''  ''hl.dsp.focus({ direction = "down" })'')
      (mk ''mod .. " + up"''    ''hl.dsp.focus({ direction = "up" })'')
      (mk ''mod .. " + right"'' ''hl.dsp.focus({ direction = "right" })'')

      # Window movement
      (mk ''mod .. " + SHIFT + H"'' ''hl.dsp.window.move({ direction = "left" })'')
      (mk ''mod .. " + SHIFT + J"'' ''hl.dsp.window.move({ direction = "down" })'')
      (mk ''mod .. " + SHIFT + K"'' ''hl.dsp.window.move({ direction = "up" })'')
      (mk ''mod .. " + SHIFT + L"'' ''hl.dsp.window.move({ direction = "right" })'')

      # Window control
      (mk ''mod .. " + ALT + H"''   ''hl.dsp.window.move({ workspace = "-1", follow = true })'')
      (mk ''mod .. " + ALT + L"''   ''hl.dsp.window.move({ workspace = "+1", follow = true })'')
      (mk ''mod .. " + SHIFT + C"'' "hl.dsp.window.close()")
      (mk ''mod .. " + F"''         "hl.dsp.window.fullscreen()")
      (mk ''mod .. " + T"''         ''hl.dsp.window.float({ action = "toggle" })'')
      (mk ''mod .. " + mouse:274"'' "hl.dsp.window.close()")

      # Dwindle layout control
      (mk ''mod .. " + ALT + R"'' ''hl.dsp.layout("togglesplit")'')
      # toggle for equal thirds ratios
      (mk ''mod .. " + R"'' ''function()
        local win = hl.get_active_window()
        if not win then return end
        local wins = hl.get_workspace_windows(win.workspace)
        local tiled = {}
        for _, w in ipairs(wins) do
          if not w.floating and not w.hidden then
            table.insert(tiled, w)
          end
        end
        if #tiled < 3 then return end
        table.sort(tiled, function(a, b)
          local ax = type(a.at) == "table" and a.at.x or a.at
          local bx = type(b.at) == "table" and b.at.x or b.at
          return ax < bx
        end)
        if _thirds_active then
          for _, w in ipairs(tiled) do
            hl.dispatch(hl.dsp.focus({ window = w }))
            hl.dispatch(hl.dsp.layout("splitratio 1.0 exact"))
          end
          _thirds_active = false
        else
          hl.dispatch(hl.dsp.focus({ window = tiled[1] }))
          hl.dispatch(hl.dsp.layout("splitratio 0.666 exact"))
          hl.dispatch(hl.dsp.focus({ window = tiled[2] }))
          hl.dispatch(hl.dsp.layout("splitratio 1.0 exact"))
          hl.dispatch(hl.dsp.focus({ window = tiled[3] }))
          hl.dispatch(hl.dsp.layout("splitratio 1.333 exact"))
          hl.dispatch(hl.dsp.focus({ window = tiled[2] }))
          hl.dispatch(hl.dsp.layout("splitratio 1.0 exact"))
          _thirds_active = true
        end
        hl.dispatch(hl.dsp.focus({ window = win }))
      end'')

      # Scrolling layout control
      (mk ''mod .. " + SHIFT + R + J"'' ''hl.dsp.layout("colresize -conf")'')
      (mk ''mod .. " + SHIFT + R + K"'' ''hl.dsp.layout("colresize +conf")'')
      (mk ''mod .. " + SHIFT + R + H"'' ''hl.dsp.layout("move -col")'')
      (mk ''mod .. " + SHIFT + R + L"'' ''hl.dsp.layout("move +col")'')

      # Workspace navigation
      (mk ''mod .. " + CTRL + H"'' ''hl.dsp.focus({ workspace = "-1" })'')
      (mk ''mod .. " + CTRL + L"'' ''hl.dsp.focus({ workspace = "+1" })'')
      (mk ''mod .. " + 1"'' "hl.dsp.focus({ workspace = 1 })")
      (mk ''mod .. " + 2"'' "hl.dsp.focus({ workspace = 2 })")
      (mk ''mod .. " + 3"'' "hl.dsp.focus({ workspace = 3 })")
      (mk ''mod .. " + 4"'' "hl.dsp.focus({ workspace = 4 })")
      (mk ''mod .. " + 5"'' "hl.dsp.focus({ workspace = 5 })")
      (mk ''mod .. " + 6"'' "hl.dsp.focus({ workspace = 6 })")
      (mk ''mod .. " + 7"'' "hl.dsp.focus({ workspace = 7 })")
      (mk ''mod .. " + 8"'' "hl.dsp.focus({ workspace = 8 })")
      (mk ''mod .. " + 9"'' "hl.dsp.focus({ workspace = 9 })")
      (mk ''mod .. " + 0"'' "hl.dsp.focus({ workspace = 10 })")

      # Move window to workspace (follow focus)
      (mk ''mod .. " + SHIFT + 1"'' "hl.dsp.window.move({ workspace = 1, follow = true })")
      (mk ''mod .. " + SHIFT + 2"'' "hl.dsp.window.move({ workspace = 2, follow = true })")
      (mk ''mod .. " + SHIFT + 3"'' "hl.dsp.window.move({ workspace = 3, follow = true })")
      (mk ''mod .. " + SHIFT + 4"'' "hl.dsp.window.move({ workspace = 4, follow = true })")
      (mk ''mod .. " + SHIFT + 5"'' "hl.dsp.window.move({ workspace = 5, follow = true })")
      (mk ''mod .. " + SHIFT + 6"'' "hl.dsp.window.move({ workspace = 6, follow = true })")
      (mk ''mod .. " + SHIFT + 7"'' "hl.dsp.window.move({ workspace = 7, follow = true })")
      (mk ''mod .. " + SHIFT + 8"'' "hl.dsp.window.move({ workspace = 8, follow = true })")
      (mk ''mod .. " + SHIFT + 9"'' "hl.dsp.window.move({ workspace = 9, follow = true })")
      (mk ''mod .. " + SHIFT + 0"'' "hl.dsp.window.move({ workspace = 10, follow = true })")

      # Move window to workspace silently (no focus follow)
      (mk ''mod .. " + SHIFT + CTRL + 1"'' "hl.dsp.window.move({ workspace = 1, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 2"'' "hl.dsp.window.move({ workspace = 2, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 3"'' "hl.dsp.window.move({ workspace = 3, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 4"'' "hl.dsp.window.move({ workspace = 4, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 5"'' "hl.dsp.window.move({ workspace = 5, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 6"'' "hl.dsp.window.move({ workspace = 6, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 7"'' "hl.dsp.window.move({ workspace = 7, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 8"'' "hl.dsp.window.move({ workspace = 8, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 9"'' "hl.dsp.window.move({ workspace = 9, follow = false })")
      (mk ''mod .. " + SHIFT + CTRL + 0"'' "hl.dsp.window.move({ workspace = 10, follow = false })")

      # Monitor navigation
      (mk ''mod .. " + comma"''          ''hl.dsp.focus({ monitor = "-1" })'')
      (mk ''mod .. " + period"''         ''hl.dsp.focus({ monitor = "+1" })'')
      (mk ''mod .. " + CTRL + comma"''   ''hl.dsp.workspace.move({ monitor = "-1" })'')
      (mk ''mod .. " + CTRL + period"''  ''hl.dsp.workspace.move({ monitor = "+1" })'')
      (mk ''mod .. " + SHIFT + comma"''  ''hl.dsp.workspace.swap_monitors({ monitor1 = "0", monitor2 = "1" })'')
      (mk ''mod .. " + SHIFT + period"'' ''hl.dsp.workspace.swap_monitors({ monitor1 = "1", monitor2 = "0" })'')

      # Window resize (repeating)
      (mkOpts ''mod .. " + SHIFT + left"''  "hl.dsp.window.resize({ x = -5, y = 0, relative = true })" { repeating = true; })
      (mkOpts ''mod .. " + SHIFT + right"'' "hl.dsp.window.resize({ x = 5, y = 0, relative = true })"  { repeating = true; })
      (mkOpts ''mod .. " + SHIFT + up"''    "hl.dsp.window.resize({ x = 0, y = -5, relative = true })" { repeating = true; })
      (mkOpts ''mod .. " + SHIFT + down"''  "hl.dsp.window.resize({ x = 0, y = 5, relative = true })"  { repeating = true; })

      # Mouse window control
      (mkOpts ''mod .. " + mouse:272"''        "hl.dsp.window.drag()"   { mouse = true; })
      (mkOpts ''mod .. " + SHIFT + mouse:272"'' "hl.dsp.window.resize()" { mouse = true; })

      # Media keys (locked — work on lockscreen too)
      (mkExecOpts ''"XF86AudioMute"''    "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"   { locked = true; })
      (mkExecOpts ''"XF86AudioMicMute"'' "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle" { locked = true; })
      (mkExecOpts ''"XF86AudioPlay"''    "playerctl play-pause"  { locked = true; })
      (mkExecOpts ''"XF86AudioPrev"''    "playerctl position -5" { locked = true; })
      (mkExecOpts ''"XF86AudioNext"''    "playerctl position +5" { locked = true; })

      # Media keys — previous/next track (once, normal mode)
      (mkExecOpts ''"XF86AudioPrev"'' "playerctl previous" { once = true; })
      (mkExecOpts ''"XF86AudioNext"'' "playerctl next"     { once = true; })

      # Volume / brightness (locked + repeating)
      (mkExecOpts ''"XF86AudioRaiseVolume"''  "wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 1%+" { locked = true; repeating = true; })
      (mkExecOpts ''"XF86AudioLowerVolume"''  "wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 1%-" { locked = true; repeating = true; })
      (mkExecOpts ''"XF86MonBrightnessUp"''   "hyprctl hyprsunset gamma +2"   { locked = true; repeating = true; })
      (mkExecOpts ''"XF86MonBrightnessDown"'' "hyprctl hyprsunset gamma -2"   { locked = true; repeating = true; })
      (mkExecOpts ''mod .. " + SHIFT + apostrophe"'' "hyprctl hyprsunset temperature +100" { locked = true; repeating = true; })
      (mkExecOpts ''mod .. " + SHIFT + Escape"''     "hyprctl hyprsunset temperature -100" { locked = true; repeating = true; })
    ];


  };
}
