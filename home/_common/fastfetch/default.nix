{config, ...}: {
  home.file = {
    "${config.xdg.configHome}/fastfetch/logo.png".source = ../../../assets/logos/phd-ark-512.png;
  };
  
  programs.fastfetch = {
    enable = true;
    settings = {
      logo = {
        "type" = "auto";
        "source" = "${config.xdg.configHome}/fastfetch/logo.png";
      };
      display = {
        color = "bright_light_yellow";
      };
      modules = [
        {
          "type" = "title";
          "format" = "{#magenta}{user-name}{#reset_}@{#blue}{host-name}";
        }
        "separator"
        "host"
        "os"
        "kernel"
        "shell"
        "editor"
        "wm"
        "terminal"
        {
          "type" = "cpu";
          "format" = "{name}";
        }
        {
          "type" = "gpu";
          "format" = "{name} {shared-total}";
        }
        "memory"
        "swap"
        {
          "type" = "disk";
          "format" = "{size-total} ({size-percentage}) [{filesystem}]";
        }
      ];
    };
  };

  # programs.zsh. = {}
}
