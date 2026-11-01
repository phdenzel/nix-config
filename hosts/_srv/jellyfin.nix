{pkgs, lib, inputs, system, ...}: {
  users.users.jellyfin.extraGroups = [ "media" ];
  services.jellyfin = {
    enable = true;
    # Jellyfin 12 only migrates cleanly from >= 10.11.11; drop this pin after one deploy on 10.11.11
    package = inputs.nixpkgs-stable.legacyPackages.${system}.jellyfin;
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
