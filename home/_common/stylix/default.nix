{pkgs, lib, ...}: {
  imports = [
    ./iridis.nix
    ./targets.nix
  ];

  stylix = {
    opacity.popups = 0.9;
    opacity.terminal = 0.9;
    polarity = lib.mkForce "dark";

    fonts = {
      monospace = {
        name = "JetBrainsMono Nerd Font";
        package = pkgs.nerd-fonts.jetbrains-mono;
      };
      serif.name = "Noto Serif";
      sansSerif.name = "Noto Sans";
      emoji = {
        name = "Noto Color Emoji";
        package = pkgs.noto-fonts-color-emoji;
      };
      sizes = {
        applications = 10;
        desktop = 10;
        terminal = 10;
      };
    };

    icons = {
      enable = true;
      dark = "breeze-dark";
      light = "breeze";
      package = pkgs.kdePackages.breeze-icons;
    };
  };
}
