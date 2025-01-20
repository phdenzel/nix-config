{inputs, config, ...}: {
  home.file.wallpapers = {
    source = inputs.phd-wallpapers;
    target = config.xdg.userDirs.pictures + "/wallpapers";
  };
  stylix = {
    image = config.xdg.userDirs.pictures + "/wallpapers/ethereal_4k.png";
    imageScalingMode = "fill";
  };
}
