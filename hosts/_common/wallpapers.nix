{inputs, pkgs, ...}: let
  phd-wallpapers = pkgs.runCommand "phd-wallpapers" {} ''
    mkdir -p $out/share/backgrounds
    ln -s ${inputs.phd-wallpapers} $out/share/backgrounds/nixos
  '';
in {
  environment.systemPackages = [ phd-wallpapers ];
  environment.pathsToLink = [ "/share/backgrounds" ];
}
