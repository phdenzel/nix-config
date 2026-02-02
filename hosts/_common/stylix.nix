{
  inputs,
  lib,
  ...
}:
with lib; {
  imports = [
    inputs.stylix.nixosModules.stylix
    ./wallpapers.nix
    ../../home/_common/stylix/iridis.nix
  ];
  # Stylix host defaults (required settings only)
  stylix.enable = mkDefault true;
  stylix.autoEnable = mkDefault false;
  stylix.image = mkDefault (inputs.phd-wallpapers + ./within_everything_4k.png);
  stylix.polarity = mkDefault "dark";
  stylix.targets = {
    console.enable = true;
    grub.enable = true;
    gtk.enable = true;
  };
}
