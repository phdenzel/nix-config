{
  inputs,
  lib,
  ...
}:
with lib; {
  imports = [
    inputs.stylix.nixosModules.stylix
    ../../home/_common/stylix/iridis.nix
  ];
  # Stylix host defaults (required settings only)
  stylix.enable = mkDefault true;
  stylix.autoEnable = mkDefault false;
  stylix.image = mkDefault ../../home/_common/stylix/nixos-default-wallpaper.png;
  stylix.polarity = mkDefault "dark";
  stylix.targets = {
    console.enable = true;
    grub.enable = true;
  };
}
