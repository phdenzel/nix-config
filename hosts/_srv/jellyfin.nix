{...}: {
  services.jellyfin = {
    enable = true;
    openFirewall = true;  # 8096, 8920
    # cacheDir = "/var/cache/jellyfin";
    # dataDir = "/var/lib/jellyfin";
    # configDir = "/var/lib/jellyfin/config";
    # logDir = "/var/lib/jellyfin/log";
  };
}
