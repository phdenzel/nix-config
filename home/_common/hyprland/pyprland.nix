{pkgs, ...}: let
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

    [scratchpads.clipboard]
    animation = "fromTop"
    command = "${termExec "clipboard"} -e clipse"
    class = "scratchpad-clipboard"
    position = "30% 10%"
    size = "75% 88%"
    max_size = "1920px 100%"
  '';
}
