{lib, ...}: {
  wayland.windowManager.hyprland.settings = {
    on = {
      _args = [
        "hyprland.start"
        (lib.generators.mkLuaInline ''function()
          hl.exec_cmd("uwsm finalize")
          hl.exec_cmd("pypr --debug /tmp/pypr.log")
          hl.exec_cmd("waybar")
          hl.exec_cmd("hyprsunset")
          hl.exec_cmd("udiskie")
          hl.exec_cmd("lact daemon")
        end'')
      ];
    };
  };
}
