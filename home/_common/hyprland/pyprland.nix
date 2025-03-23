{...}: {
  home.file.".config/hypr/pyprland.toml".text = ''
    [pyprland]
    plugins = [
      "scratchpads",
    ]

    [pyprland.variables]
    uwsm_term = "uwsm app -T --"
    
    [scratchpads.term]
    animation = ""
    # animation = "fromTop"
    command = "[uwsm_term]"
    class = "scratchpad-term"
    position = "50% 5%"
    size = "75% 60%"
    max_size = "1920px 100%"

    [scratchpads.filemgr]
    animation = ""
    # animation = "fromTop"
    command = "[uwsm_term] yazi"
    class = "scratchpad-filemgr"
    position = "50% 5%"
    size = "75% 60%"
    max_size = "1920px 100%"
  '';
}
