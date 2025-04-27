{...}: {
  fileSystems."/nfs/data/phdenzel" = {
    device = "192.168.178.42:/data/phdenzel";
    fsType = "nfs";
  };

  fileSystems."/nfs/data/media" = {
    device = "192.168.178.42:/data/media";
    fsType = "nfs";
  };

  fileSystems."/nfs/data/documents" = {
    device = "192.168.178.42:/data/documents";
    fsType = "nfs";
  };
}
