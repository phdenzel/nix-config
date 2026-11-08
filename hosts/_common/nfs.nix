{...}: let
  options = [
    "nfsvers=4.2"
    "noatime"
    "noauto"
    "x-systemd.automount"
    "x-systemd.idle-timeout=600"
    "x-systemd.mount-timeout=15"
    "_netdev"
  ];
  share = name: extra: {
    device = "ygdrasil.home:/data/${name}";
    fsType = "nfs";
    options = options ++ extra;
  };
in {
  fileSystems."/nfs/data/media" = share "media" [];
  fileSystems."/nfs/data/documents" = share "documents" [];
  fileSystems."/nfs/data/store" = share "store" ["ro"];
}
