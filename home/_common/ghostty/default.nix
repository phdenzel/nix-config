{config, ...}: {
  programs.ghostty = {
    enable = true;
    settings = {
      theme = "${config.colorScheme.slug}";
      cursor-style = "block";
      shell-integration-features = "no-cursor";
      mouse-hide-while-typing = true;
      window-height = 48;
      window-width = 84;
    };
    themes = with config.colorScheme; {
      "${slug}" = {
        background = "#${palette.base}";
        cursor-color = "#${palette.blue}";
        foreground = "#${palette.text}";
        selection-background = "#${palette.overlay1}";
        selection-foreground = "#${palette.white}";
        palette = [
          "0=#${palette.base}"
          "1=#${palette.red}"
          "2=#${palette.green}"
          "3=#${palette.yellow}"
          "4=#${palette.blue}"
          "5=#${palette.pink}"
          "6=#${palette.teal}"
          "7=#${palette.subtext0}"
          "8=#${palette.surface1}"
          "9=#${palette.ruby}"
          "10=#${palette.viridis}"
          "11=#${palette.sand}"
          "12=#${palette.indigo}"
          "13=#${palette.magenta}"
          "14=#${palette.cyan}"
          "15=#${palette.white}"
        ];
      };
    };
  };
}
