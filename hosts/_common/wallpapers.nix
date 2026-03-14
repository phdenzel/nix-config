{inputs, pkgs, ...}: let
  phd-wallpapers = pkgs.runCommand "phd-wallpapers" {} ''
    mkdir -p $out/share/backgrounds
    ln -s ${inputs.phd-wallpapers} $out/share/backgrounds/nixos
  '';
in {
  # TODO: fix by adding phd-wallpapers to environment.systemPackages and environment.pathsToLink = ["/share/backgrounds/nixos"] https://discourse.nixos.org/t/is-there-a-permanent-folder-for-official-wallpapers/58945
  environment.systemPackages = [ phd-wallpapers ];
  environment.pathsToLink = [ "share/backgrounds" ];
  environment.etc = {
    wallpapers = {
      source = inputs.phd-wallpapers;
    };
  };
}
