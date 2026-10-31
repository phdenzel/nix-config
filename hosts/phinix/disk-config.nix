# This is the disko configuration for my workstation
# From a set of 1-6 NVME drives, it creates a
# 1) "nixos" volume with an ESP, swap, and a btrfs partition (subvol @, @home, @nix, @var, @snapshots)
# 2) "scratch" btrfs volume where miscellaneous games, containers, and VMs are stored (subvol @scratch, @games, @virt)
# 3-6) "raid1" volume (btrfs raid with data as raid0 and metadata as raid1 profile) where datasets are stored
# Devices in phinix:
# - nixos:   /dev/disk/by-id/nvme-CT4000T700SSD3_2341E87F4711
# - scratch: /dev/disk/by-id/nvme-WD_BLACK_SN850X_4000GB_23322S800994
# - raid1:   /dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135359K
# - raid1:   /dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNU0X405324H
# - raid1:   /dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135355N
# - raid1:   /dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135331M
{...}: let
  disks = [
    "/dev/disk/by-id/nvme-CT4000T700SSD3_2341E87F4711"
    "/dev/disk/by-id/nvme-WD_BLACK_SN850X_4000GB_23322S800994"
    "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135359K"
    "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNU0X405324H"
    "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135355N"
    "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135331M"
  ];
  numberOfDisks =
    if (builtins.length disks > 6)
    then throw "Error. For this scheme a maximum of 6 disks can be passed to disko."
    else builtins.length disks;
  nvme = {
    a = builtins.elemAt disks 0;
    b =
      if (numberOfDisks > 1)
      then builtins.elemAt disks 1
      else "";
    c =
      if (numberOfDisks > 2)
      then builtins.elemAt disks 2
      else "";
    d =
      if (numberOfDisks > 3)
      then builtins.elemAt disks 3
      else "";
    e =
      if (numberOfDisks > 4)
      then builtins.elemAt disks 4
      else "";
    f =
      if (numberOfDisks > 5)
      then builtins.elemAt disks 5
      else "";
  };
  raidMember = n: "/dev/disk/by-partlabel/raid1p${toString n}";
  raidPartitions =
    {
      "1" = ""; # not enough disks for raid setup
      "2" = ""; # not enough disks for raid setup
      "3" = ""; # not enough disks for raid setup
      "4" = "${raidMember 1}"; # raid1p2 is the partition where the raid is initialized
      "5" = "${raidMember 1} ${raidMember 2}"; # raid1p3 is the partition where the raid is initialized
      "6" = "${raidMember 1} ${raidMember 2} ${raidMember 3}"; # raid1p4 is the partition where the raid is initialized
    }
    ."${builtins.toString numberOfDisks}";
in {
  disko.devices = {
    disk = {
      nixos = {
        type = "disk";
        device = "${nvme.a}";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              label = "EFI";
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
              size = "32G";
              content = {
                type = "swap";
                extraArgs = ["-L swap"];
                # randomEncryption = true;
              };
            };
            root = {
              label = "root";
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

      scratch =
        if (numberOfDisks < 2)
        then {}
        else {
          type = "disk";
          device = "${nvme.b}";
          content = {
            type = "gpt";
            partitions = {
              scratch = {
                label = "scratch";
                start = "64M";
                size = "100%";
                content = {
                  type = "btrfs";
                  extraArgs = ["-L scratch"];
                  subvolumes = {
                    "@scratch" = {
                      mountpoint = "/scratch";
                      mountOptions = ["subvol=@scratch" "compress=zstd" "noatime"];
                    };
                    "@games" = {
                      mountpoint = "/scratch/games";
                      mountOptions = ["subvol=@games" "compress=zstd" "noatime"];
                    };
                    "@virt" = {
                      mountpoint = "/scratch/virt";
                      mountOptions = ["subvol=@virt" "compress=zstd" "noatime"];
                    };
                  };
                };
              };
            };
          };
        };

      raid1p4 =
        if (numberOfDisks < 6)
        then {}
        else {
          type = "disk";
          device = "${nvme.f}";
          content = {
            type = "gpt";
            partitions = {
              raid1p4 = {
                label = "raid1p4";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 6
                  then {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L raid1"
                      "-d RAID0"
                      "-m RAID1"
                      "${raidPartitions}"
                    ];
                    mountpoint = "/raid";
                    mountOptions = ["compress=zstd" "noatime"];
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };

      raid1p3 =
        if (numberOfDisks < 5)
        then {}
        else {
          type = "disk";
          device = "${nvme.e}";
          content = {
            type = "gpt";
            partitions = {
              raid1p3 = {
                label = "raid1p3";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 5
                  then {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L raid1"
                      "-d RAID0"
                      "-m RAID1"
                      "${raidPartitions}"
                    ];
                    mountpoint = "/raid";
                    mountOptions = ["compress=zstd" "noatime"];
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };

      raid1p2 =
        if (numberOfDisks < 4)
        then {}
        else {
          type = "disk";
          device = "${nvme.d}";
          content = {
            type = "gpt";
            partitions = {
              raid1p2 = {
                label = "raid1p2";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 4
                  then {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L raid1"
                      "-d RAID0"
                      "-m RAID1"
                      "${raidPartitions}"
                    ];
                    mountpoint = "/raid";
                    mountOptions = ["compress=zstd" "noatime"];
                  }
                  else {
                    type = "btrfs";
                  };
              };
            };
          };
        };

      raid1p1 =
        if (numberOfDisks < 3)
        then {}
        else {
          type = "disk";
          device = "${nvme.c}";
          content = {
            type = "gpt";
            partitions = {
              raid1p1 = {
                label = "raid1p1";
                start = "64M";
                size = "100%";
                content =
                  if numberOfDisks == 3
                  then {
                    type = "btrfs";
                    mountpoint = "/raid";
                    mountOptions = ["compress=zstd" "noatime"];
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
