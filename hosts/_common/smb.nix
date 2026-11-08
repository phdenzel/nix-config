{config, ...}: let
  options = [
    "credentials=${config.sops.templates."smb-credentials".path}"
    "vers=3.1.1"
    "seal"
    "uid=phdenzel"
    "gid=users"
    "file_mode=0600"
    "dir_mode=0700"
    "nosuid"
    "nodev"
    "noauto"
    "x-systemd.automount"
    "x-systemd.idle-timeout=600"
    "x-systemd.mount-timeout=15"
    "_netdev"
  ];
  share = name: {
    device = "//ygdrasil.home/${name}";
    fsType = "cifs";
    inherit options;
  };
in {
  fileSystems."/nfs/data/private" = share "private";
  fileSystems."/nfs/data/backups" = share "backups";

  sops-host.keys = ["samba/phdenzel"];
  sops.templates."smb-credentials".content = ''
    username=phdenzel
    password=${config.sops.placeholder."samba/phdenzel"}
  '';
}
