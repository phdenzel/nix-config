{lib, ...}: let
  clients = [
    "192.168.178.188" # sol
    "192.168.178.156" # phinix
    "192.168.178.138" # fenrix
    "192.168.178.140" # fenrix
  ];
  export = options: lib.genAttrs clients (_: options ++ ["sync" "no_subtree_check" "root_squash"]);
in {
  services.nfs = {
    server = {
      enable = true;
      exports = {
        "/data/media" = export ["rw" "fsid=1"];
        "/data/documents" = export ["rw" "fsid=2"];
        "/data/store" = export ["ro" "fsid=3"];
      };
    };
    settings.nfsd = {
      vers3 = false;
      "vers4.0" = false;
    };
  };

  networking.firewall.allowedTCPPorts = [2049];
}
