{pkgs, config, ...}: let
  palette = config.colorScheme.palette;
in {
  imports = [
    ./rofi-pass.nix
  ];

  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    plugins = with pkgs; [rofi-file-browser rofi-calc rofi-emoji-wayland];
    extraConfig = {
      modi = "drun,run,window,filebrowser,emoji,calc";
      show-icons = true;
      terminal = "ghostty";
      drun-display-format = "{icon} {name}";
      disable-history = false;
      hide-scrollbar = true;
      display-drun = " Apps";
      display-run = " Run";
      display-window = "󱂬 Windows";
      display-filebrowser = " Files";
      display-emoji = " Emoji";
      display-calc = " Calc";
      sidebar-mode = true;
      calc-command = "echo -h '{result} | wl-copy'";
    };
    location = "center";
    theme = let # will be symlinked to ~/.local/share/rofi/themes/custom.rasi
      inherit (config.lib.formats.rasi) mkLiteral;
      backgroundColor = mkLiteral "@bgColor";
      buttonColor = mkLiteral "@bgHColor";
      borderColor = mkLiteral "@bgColor";
      foregroundColor = mkLiteral "@fgColor";
      outlineColor = mkLiteral "@fgHColor";
      accentColor = mkLiteral "@acColor";
      secondaryColor = mkLiteral "@seColor";
    in {
      "*" = {
        bgColor = mkLiteral "#${palette.base}";
        bgHColor = mkLiteral "#${palette.overlay1}";
        fgColor = mkLiteral "#${palette.text}";
        fgHColor = mkLiteral "#${palette.white}";
        acColor = mkLiteral "#${palette.blue}";
        seColor = mkLiteral "#${palette.pink}";
        width = 800;
      };

      "element-text, element-icon, mode-switcher" = {
        background-color = mkLiteral "inherit";
        text-color = mkLiteral "inherit";
      };

      window = {
        background-color = backgroundColor;
        border-color = borderColor;
        border-radius = mkLiteral "6px";
        border = mkLiteral "3px";
        height = mkLiteral "520px";
      };

      mainbox = {
        background-color = backgroundColor;
      };

      inputbar = {
        background-color = backgroundColor;
        border-radius = mkLiteral "8px";
        children = map mkLiteral ["prompt" "entry"];
        padding = mkLiteral "2px";
      };

      prompt = {
        background-color = accentColor;
        border-radius = mkLiteral "6px";
        margin = mkLiteral "20px 0px 0px 20px";
        padding = mkLiteral "6px";
        text-color = foregroundColor;
      };

      textbox-prompt-colon = {
        expand = false;
        str = ":";
      };

      entry = {
        background-color = backgroundColor;
        margin = mkLiteral "20px 0px 0px 10px";
        padding = mkLiteral "6px";
        text-color = foregroundColor;
      };

      listview = {
        background-color = backgroundColor;
        border = mkLiteral "0px 0px 0px";
        columns = 2;
        lines = 5;
        margin = "10px 0px 0px 20px";
        padding = mkLiteral "6px 0px 0px";
      };

      element = {
        background-color = backgroundColor;
        padding = mkLiteral "5px";
        text-color = foregroundColor;
      };

      element-icon = {
        size = mkLiteral "25px";
      };

      "element selected" = {
        background-color = backgroundColor;
        text-color = secondaryColor;
      };

      mode-switcher = {
        spacing = 0;
      };

      button = {
        background-color = buttonColor;
        horizontal-align = mkLiteral "0.5";
        vertical-align = mkLiteral "0.5";
        padding = mkLiteral "10px";
        text-color = outlineColor;
      };

      "button selected" = {
        background-color = backgroundColor;
        text-color = accentColor;
      };

      message = {
        background-color = backgroundColor;
        border-radius = mkLiteral "8px";
        margin = mkLiteral "2px";
        padding = mkLiteral "2px";
      };

      textbox = {
        background-color = backgroundColor;
        margin = mkLiteral "20px 0px 0px 20px";
        padding = mkLiteral "6px";
        text-color = accentColor;
      };
    };
  };
}
