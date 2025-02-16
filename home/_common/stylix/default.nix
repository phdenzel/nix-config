{pkgs, ...}: {
  imports = [
    ./iridis.nix
    ./targets.nix
  ];

  stylix = {
    opacity.popups = 0.9;
    opacity.terminal = 0.9;

    fonts.monospace = {
      name = "JetBrainsMono Nerd Font";
      package = pkgs.nerd-fonts.jetbrains-mono;
    };
  };
}
