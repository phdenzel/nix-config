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
{lib, ...}: let
  disks = [
    "/dev/nvme0n1"
    "/dev/sda"
    "/dev/sdb"
    "/dev/sdc"
    "/dev/sdd"
    "/dev/sde"
    "/dev/sdf"
  ];
  numberOfDisks =
    if (builtins.length disks > 7)
    then throw "Error. For this scheme a maximum of 7 disks can be passed to disko."
    else builtins.length disks;
  nvme = rec {
    device = builtins.elemAt disks 0;
    p1 =
      if (builtins.substring 0 9 device == "/dev/nvme")
      then "${device}p1"
      else "${device}-part1";
    p2 =
      if (builtins.substring 0 9 device == "/dev/nvme")
      then "${device}p2"
      else "${device}-part2";
    p3 =
      if (builtins.substring 0 9 device == "/dev/nvme")
      then "${device}p3"
      else "${device}-part3";
  };
  nas = {
    a = builtins.elemAt disks 1;
    b =
      if (numberOfDisks > 2)
      then builtins.elemAt disks 2
      else "";
    c =
      if (numberOfDisks > 3)
      then builtins.elemAt disks 3
      else "";
    d =
      if (numberOfDisks > 4)
      then builtins.elemAt disks 4
      else "";
    e =
      if (numberOfDisks > 5)
      then builtins.elemAt disks 5
      else "";
    f =
      if (numberOfDisks > 6)
      then builtins.elemAt disks 6
      else "";
  };
  nasExtraDevices =
    {
      "1" = "";
      "2" = "";
      "3" = "${nas.b}";
      "4" = "${nas.b} ${nas.c}";
      "5" = "${nas.b} ${nas.c} ${nas.d}";
      "6" = "${nas.b} ${nas.c} ${nas.d} ${nas.e}";
      "7" = "${nas.b} ${nas.c} ${nas.d} ${nas.e} ${nas.f}";
    }
      ."${builtins.toString numberOfDisks}";
  nasSubvolumes = {
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
    "@store" = {
      mountpoint = "/data/store";
      mountOptions = ["subvol=@store" "compress=zstd" "noatime"];
    };
  };
  nasRaidContent = {
    type = "btrfs";
    # disko appends nas.a automatically; extra devices are listed here.
    # mkfs.btrfs formats all devices in one call, so nas2-nas6 are NOT
    # declared as separate disko disk entries.
    extraArgs =
      [
        "-f"
        "-L" "nas1"
        "-d" "RAID0"
        "-m" "RAID1"
      ]
      ++ (lib.splitString " " nasExtraDevices);
    subvolumes = nasSubvolumes;
  };
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
                mountOptions = ["umask=0077"];
              };
            };
            swap = {
              label = "swap";
              device = "${nvme.p2}";
              size = "32G";
              content = {
                type = "swap";
                extraArgs = ["-L" "swap"];
                # randomEncryption = true;
              };
            };
            root = {
              label = "root";
              device = "${nvme.p3}";
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = ["-L" "root"];
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

      nas1 =
        if (numberOfDisks < 2)
        then {}
        else {
          type = "disk";
          device = "${nas.a}";
          content =
            if numberOfDisks == 2
            then {
              type = "btrfs";
              extraArgs = ["-f" "-L" "nas1"];
              subvolumes = nasSubvolumes;
            }
            else nasRaidContent;
        };
    };
  };
}
