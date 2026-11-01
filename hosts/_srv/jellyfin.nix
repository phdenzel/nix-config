{pkgs, lib, ...}: {
  users.users.jellyfin.extraGroups = [ "media" ];
  services.jellyfin = {
    enable = true;
    # openFirewall = true;  # 8096, 8920
    hardwareAcceleration = lib.mkDefault {
      enable = true;
      type = "vaapi";
      device = "/dev/dri/renderD128";
    };
  };

  environment.systemPackages = with pkgs; [
    libva-utils
  ];
}
