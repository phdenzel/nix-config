{pkgs, ...}: let
  uwsmExec = program: "uwsm app -- ghostty --title=scratchpad.${program}";
  termExec = program: "uwsm app -- ghostty --title=scratchpad.${program}";
in {
  home.packages = with pkgs; [pyprland];
  
  home.file.".config/hypr/pyprland.toml".text = ''
    [pyprland]
    plugins = [
      "scratchpads",
    ]
    
    [scratchpads.term]
    animation = "fromTop"
    command = "${termExec "term"}"
    class = "scratchpad-term"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"

    [scratchpads.filemgr]
    animation = "fromTop"
    command = "${termExec "filemgr"} -e yazi"
    class = "scratchpad-filemgr"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"

    [scratchpads.monitor]
    animation = "fromTop"
    command = "${termExec "monitor"} -e btop"
    class = "scratchpad-monitor"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"

    [scratchpads.volmgr]
    animation = "fromTop"
    command = "${uwsmExec "volmgr"} -e pavucontrol"
    class = "scratchpad-volmgr"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"

    [scratchpads.clipboard]
    animation = "fromTop"
    command = "${termExec "clipboard"} -e clipse"
    class = "scratchpad-clipboard"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"
  '';
}
