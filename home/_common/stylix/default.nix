{inputs, ...}: {
  imports = [
    ./iridis.nix
    ./targets.nix
  ];

  stylix = {
    opacity.popups = 0.9;
    opacity.terminal = 0.9;
  };
}
