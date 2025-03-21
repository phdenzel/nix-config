{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    foliate
    gimp
    imagemagick
    inkscape
    imv
    libreoffice-fresh
    mpv
    kdePackages.okular
    pdfarranger
    zathura
  ];
}
