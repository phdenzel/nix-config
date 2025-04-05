{config, ...}: let
  palette = config.colorScheme.palette;
in {
  programs.hyprlock = {
    enable = true;

    settings = {
      background = [
        {
          monitor = "";
          path = config.stylix.image;
          color = "rgb(${palette.base})";
          blur_passes = 2;
          blur_size = 7;
          noise = 0.0117;
          brightness = 0.6;
          vibrancy = 0.16;
          vibrancy_darkness = 0.0;
        }
      ];

      input-field = [
        {
          monitor = "";
          size = "350, 80";
          position = "0, 600";
          halign = "center";
          valign = "bottom";
          outline_thinkness = 3;
          dots_size = 0.2;
          dots_spacing = 0.64;
          dots_center = true;
          dots_rounding = -1;
          outer_color = "rgb(${palette.surface1})";
          inner_color = "rgb(${palette.base})";
          font_color = "rgb(${palette.text})";
          fade_on_empty = false;
          fade_timeout = 500;
          placeholder_text = "<i>Password...</i>";
          hide_input = false;
          rounding = 12;
          check_color = "rgb(${palette.indigo})";
          fail_color = "rgb(${palette.ruby})";
          fail_transition = 300;
          capslock_color = -1;
          numlock_color = -1;
          bothlock_color = -1;
          invert_numlock = false;
          swap_font_color = false;
        }
      ];

      label = [
        {
          monitor = "";
          text = "cmd[update:100] echo \"<b><big> $(date +\"%H:%M\") </big></b>\"";
          color = "rgb(${palette.text})";
          font_size = 96;
          font_family = "JetBrainsMono Nerd Font";
          position = "0, 180";
          halign = "center";
          valign = "center";
        }
        {
          monitor = "";
          text = "Ciao $USER";
          color = "rgb(${palette.text})";
          font_size = 32;
          position = "0, 60";
          halign = "center";
          valign = "center";
        }
        {
          monitor = "";
          text = "Type to unlock!";
          color = "rgb(${palette.text})";
          font_size = 18;
          position = "0, 520";
          halign = "center";
          valign = "bottom";
        }
      ];
    };
  };
}
