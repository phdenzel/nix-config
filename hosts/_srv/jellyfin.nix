{lib, ...}: {
  services.jellyfin = {
    enable = true;
    openFirewall = true;  # 8096, 8920
    hardwareAcceleration = lib.mkDefault {
      enable = true;
      type = "vaapi";
      device = "/dev/dri/renderD128";
    };
  };
}
