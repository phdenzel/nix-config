# This is the disko configuration for my workstation
# From a set of 1 NVME drive and 6 large HDDs, it creates a
# 1) "nixos" volume with an ESP, swap, and a btrfs partition (subvol @, @home, @nix, @var, @snapshots)
# 2-7) "nas1" volume (btrfs raid with data as raid0 and metadata as raid1 profile) for bulk NAS storage
# Devices in ygdrasil:
# - nixos:   nvme0n1 (/dev/disk/by-id/)
# - nas1:    sda (/dev/disk/by-id/)
# - nas1:    sdb (/dev/disk/by-id/)
# - nas1:    sdc (/dev/disk/by-id/)
# - nas1:    sdd (/dev/disk/by-id/)
# - nas1:    sde (/dev/disk/by-id/)
# - nas1:    sdf (/dev/disk/by-id/)
{...}: let
  disks = [
    "/dev/nvme0n1"
    "/dev/sda"
    "/dev/sdb"
    "/dev/sdc"
    "/dev/sdd"
    "/dev/sde"
  ];
  numberOfDisks =
    if (builtins.length disks > 7)
    then throw "Error. For this scheme a maximum of 7 disks can be passed to disko."
    else builtins.length disks;
  nvme = rec {
    device = builtins.elemAt disks 0;
    p1 = if (builtins.substring 0 9 device == "/dev/nvme")
         then "${device}p1"
         else "${device}-part1";
    p2 = if (builtins.substring 0 9 device == "/dev/nvme")
         then "${device}p2"
         else "${device}-part2";
    p3 = if (builtins.substring 0 9 device == "/dev/nvme")
         then "${device}p3"
         else "${device}-part3";
  };
  nas = rec {
    a = builtins.elemAt disks 1;
    b =
      if (numberOfDisks > 1)
      then builtins.elemAt disks 2
      else "";
    c =
      if (numberOfDisks > 2)
      then builtins.elemAt disks 3
      else "";
    d =
      if (numberOfDisks > 3)
      then builtins.elemAt disks 4
      else "";
    e =
      if (numberOfDisks > 4)
      then builtins.elemAt disks 5
      else "";
    f =
      if (numberOfDisks > 5)
      then builtins.elemAt disks 6
      else "";
    a1 =
      if (builtins.substring 0 9 a == "/dev/nvme")
      then "${a}p1"
      else "${a}1";
    b1 =
      if (builtins.substring 0 9 b == "/dev/nvme")
      then "${b}p1"
      else "${b}1";
    c1 =
      if (builtins.substring 0 9 c == "/dev/nvme")
      then "${c}p1"
      else "${c}1";
    d1 =
      if (builtins.substring 0 9 d == "/dev/nvme")
      then "${d}p1"
      else "${d}1";
    e1 =
      if (builtins.substring 0 9 e == "/dev/nvme")
      then "${e}p1"
      else "${e}1";
    f1 =
      if (builtins.substring 0 9 f == "/dev/nvme")
      then "${f}p1"
      else "${f}1";
  };
  nasPartitions =
    {
      "1" = "";
      "2" = "";
      "3" = "${nas.a1}";
      "4" = "${nas.a1} ${nas.b1}";
      "5" = "${nas.a1} ${nas.b1} ${nas.c1}";
      "6" = "${nas.a1} ${nas.b1} ${nas.c1} ${nas.d1}";
      "7" = "${nas.a1} ${nas.b1} ${nas.c1} ${nas.d1} ${nas.e1}";
    }
    ."${builtins.toString numberOfDisks}";
in {
  disko.devices = {
    disk = {
      nixos = {
        type = "disk";
        device = "${nvme.device}";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              label = "EFI";
              device = "${nvme.p1}";
              type = "EF00";
              start = "64M"; # A small mod8 offset, just because...
              end = "1G";
              content = {
                type = "filesystem";
                extraArgs = ["-n" "BOOT"];
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            swap = {
              label = "swap";
              device = "${nvme.p2}";
              size = "32G";
              content = {
                type = "swap";
                extraArgs = ["-L swap"];
                # randomEncryption = true;
              };
            };
            root = {
              label = "root";
              device = "${nvme.p3}";
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = ["-L root"];
                subvolumes = {
                  "@rootfs" = {
                    mountpoint = "/";
                    mountOptions = ["subvol=@rootfs" "compress=zstd" "noatime"];
                  };
                  "@nix" = {
                    mountpoint = "/nix";
                    mountOptions = ["subvol=@nix" "compress=zstd" "noatime"];
                  };
                  "@var" = {
                    mountpoint = "/var";
                    mountOptions = ["subvol=@var" "compress=zstd" "noatime"];
                  };
                  "@home" = {
                    mountpoint = "/home";
                    mountOptions = ["subvol=@home" "compress=zstd"];
                  };
                  "@snapshots" = {
                    mountpoint = "/snapshots";
                    mountOptions = ["subvol=@snapshots" "compress=zstd" "noatime"];
                  };
                };
              };
            };
          };
        };
      };

      nas6 =
        if (numberOfDisks < 7)
        then {}
        else {
          type = "disk";
          device = "${nas.f}";
          content = {
            type = "gpt";
            partitions = {
              nas6 = {
                label = "nas6";
                device = "${nas.f1}";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 7
                  then {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L nas1"
                      "-d RAID0"
                      "-m RAID1"
                      "${nasPartitions}"
                    ];
                    mountpoint = "/data";
                    mountOptions = ["compress=zstd" "noatime"];
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };

      nas5 =
        if (numberOfDisks < 6)
        then {}
        else {
          type = "disk";
          device = "${nas.e}";
          content = {
            type = "gpt";
            partitions = {
              nas5 = {
                label = "nas5";
                device = "${nas.e1}";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 6
                  then {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L nas1"
                      "-d RAID0"
                      "-m RAID1"
                      "${nasPartitions}"
                    ];
                    mountpoint = "/data";
                    mountOptions = ["compress=zstd" "noatime"];
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };

      nas4 =
        if (numberOfDisks < 5)
        then {}
        else {
          type = "disk";
          device = "${nas.d}";
          content = {
            type = "gpt";
            partitions = {
              nas4 = {
                label = "nas4";
                device = "${nas.d1}";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 5
                  then {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L nas1"
                      "-d RAID0"
                      "-m RAID1"
                      "${nasPartitions}"
                    ];
                    mountpoint = "/data";
                    mountOptions = ["compress=zstd" "noatime"];
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };

      nas3 =
        if (numberOfDisks < 4)
        then {}
        else {
          type = "disk";
          device = "${nas.c}";
          content = {
            type = "gpt";
            partitions = {
              nas3 = {
                label = "nas3";
                device = "${nas.c1}";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 4
                  then {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L nas1"
                      "-d RAID0"
                      "-m RAID1"
                      "${nasPartitions}"
                    ];
                    mountpoint = "/data";
                    mountOptions = ["compress=zstd" "noatime"];
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };

      nas2 =
        if (numberOfDisks < 3)
        then {}
        else {
          type = "disk";
          device = "${nas.b}";
          content = {
            type = "gpt";
            partitions = {
              nas2 = {
                label = "nas2";
                device = "${nas.b1}";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 3
                  then {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L nas1"
                      "-d RAID0"
                      "-m RAID1"
                      "${nasPartitions}"
                    ];
                    mountpoint = "/data";
                    mountOptions = ["compress=zstd" "noatime"];
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };

      nas1 =
        if (numberOfDisks < 2)
        then {}
        else {
          type = "disk";
          device = "${nas.a}";
          content = {
            type = "gpt";
            partitions = {
              nas1 = {
                label = "nas1";
                device = "${nas.a1}";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 2
                  then {
                    type = "btrfs";
                    subvolumes = {
                    "@data" = {
                      mountpoint = "/data";
                      mountOptions = ["subvol=@data" "compress=zstd" "noatime"];
                    };
                    "@media" = {
                      mountpoint = "/data/media";
                      mountOptions = ["subvol=@media" "compress=zstd" "noatime"];
                    };
                    "@documents" = {
                      mountpoint = "/data/documents";
                      mountOptions = ["subvol=@documents" "compress=zstd" "noatime"];
                    };
                    "@backups" = {
                      mountpoint = "/data/backups";
                      mountOptions = ["subvol=@backups" "compress=zstd" "noatime"];
                    };
                  };
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };
    };
  };
}

