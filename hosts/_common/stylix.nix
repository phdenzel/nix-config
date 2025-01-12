{inputs, lib, ...}: with lib; {
  imports = [inputs.stylix.nixosModules.stylix];
  # Stylix host defaults (required settings only)
  stylix.enable = mkDefault rue;
  stylix.image = mkDefault ../../home/_common/stylix/nixos-default-wallpaper.png;
  stylix.polarity = mkDefault "dark";
}
