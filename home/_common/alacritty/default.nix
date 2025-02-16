{lib, ...}: {
  programs.alacritty = with lib; {
    enable = true;
    settings = {
      font = let
        fontFamily = "JetBrainsMono Nerd Font";
      in {
        normal.family = mkDefault fontFamily;
        bold.family = mkDefault fontFamily;
        italic.family = mkDefault fontFamily;
      };
    };
  };
}
