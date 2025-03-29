{pkgs, ...}: let
  uwsmExec = program: "uwsm app ${program}";
  termExec = program: "uwsm app -- ghostty --title=scratchpad.${program}";
in {
  home.packages = with pkgs; [pyprland];
  
  home.file.".config/hypr/pyprland.toml".text = ''
    [pyprland]
    plugins = [
      "scratchpads",
    ]
    
    [scratchpads.term]
    animation = ""
    command = "${termExec "term"}"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"

    [scratchpads.filemgr]
    animation = ""
    command = "${termExec "filemgr"} -e yazi"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"

    [scratchpads.monitor]
    animation = "fromTop"
    command = "${termExec "monitor"} -e btop"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"

    [scratchpads.volmgr]
    animation = "fromTop"
    command = "pavucontrol"
    class = "org.pulseaudio.pavucontrol"
    title = "scratchpad.volmgr"
    lazy = true
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
