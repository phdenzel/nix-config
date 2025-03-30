{config, ...}: let
  wallpaperDir = "${config.xdg.userDirs.pictures}/wallpapers";
in {
  services.hyprpaper = {
    enable = true;
    settings = {
      preload = [
        "${wallpaperDir}/ethereal_4k.png"
        "${wallpaperDir}/noctilusent_4k.png"
        "${wallpaperDir}/nocturnal_reveries_4k.png"
        "${wallpaperDir}/within_everything_4k.png"
      ];
      wallpaper = [
        ",${wallpaperDir}/ethereal_4k.png"
      ];
    };
  };
}
