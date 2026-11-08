{config, ...}: let
  share = path: extra:
    {
      inherit path;
      "valid users" = "phdenzel";
      "read only" = "no";
      "create mask" = "0660";
      "directory mask" = "2770";
    }
    // extra;
in {
  services.samba = {
    enable = true;
    nmbd.enable = false;
    winbindd.enable = false;
    settings = {
      global = {
        "server min protocol" = "SMB3";
        "server smb encrypt" = "required";
        "smb ports" = "445";
        "disable netbios" = "yes";
        "hosts allow" = "192.168.178. 127.";
        "hosts deny" = "ALL";
        "map to guest" = "never";
        "restrict anonymous" = "2";
        "load printers" = "no";
        "printcap name" = "/dev/null";
        "disable spoolss" = "yes";
        "vfs objects" = "catia fruit streams_xattr";
        "fruit:metadata" = "stream";
        "fruit:model" = "MacSamba";
      };
      private = share "/data/private" {};
      backups = share "/data/backups" {};
      media = share "/data/media" {};
      documents = share "/data/documents" {};
      store = share "/data/store" {"read only" = "yes";};
    };
  };

  sops-host.keys = ["samba/phdenzel"];
  sops.secrets."samba/phdenzel".restartUnits = ["samba-passwords.service"];
  systemd.services.samba-passwords = {
    description = "Set Samba passwords from sops";
    wantedBy = ["samba-smbd.service"];
    before = ["samba-smbd.service"];
    serviceConfig.Type = "oneshot";
    script = ''
      pw=$(cat ${config.sops.secrets."samba/phdenzel".path})
      printf '%s\n%s\n' "$pw" "$pw" | ${config.services.samba.package}/bin/smbpasswd -s -a phdenzel
    '';
  };

  networking.firewall.allowedTCPPorts = [445];
}
