{inputs, ...}: {
  imports = [inputs.stylix.nixosModules.stylix];
  # Stylix host defaults (required settings only)
  stylix.enable = true;
  stylix.image = ../../home/_common/stylix/nixos-default-wallpaper.png;
}
