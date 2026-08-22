{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    foliate
    gapless
    gimp
    imagemagick
    inkscape
    imv
    libreoffice-stable
    mpv
    kdePackages.okular
    pdfarranger
    zathura
    zmk-studio
  ];

  programs = {
    appimage.enable = true;
  };
}
