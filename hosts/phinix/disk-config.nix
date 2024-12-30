# This is the disko configuration for my workstation
# It creates a
# - "nixos" volume with an ESP, swap, and a btrfs partition (subvol @, @home, @nix, @var, @snapshots)
# - "scratch" btrfs volume where miscellaneous games, containers, and VMs are stored (subvol @scratch, @games, @virt)
# - "raid1" volume (btrfs raid with data as raid0 and metadata as raid1 profile) where datasets are stored
# Device IDs in phinix:
# - nixos:   nvme0n1 (/dev/disk/by-id/nvme-CT4000T700SSD3_2341E87F4711)
# - scratch: nvme5n1 (/dev/disk/by-id/nvme-WD_BLACK_SN850X_4000GB_23322S800994)
# - raid1:   nvme1n1 (/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135359K -> eui.0025384141430d8c)
# - raid1:   nvme2n1 (/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNU0X405324H -> eui.0025384441a17c56)
# - raid1:   nvme3n1 (/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135355N -> eui.0025384141430d88)
# - raid1:   nvme4n1 (/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135331M -> eui.0025384141430d07)
{
  disks ? [
    "/dev/nvme0n1"
    "/dev/nvme5n1"
    "/dev/nvme1n1"
    "/dev/nvme2n1"
    "/dev/nvme3n1"
    "/dev/nvme4n1"
  ],
  ...
}: let
  numberOfDisks =
    if (builtins.length disks > 6)
    then throw "Error. For this scheme a maximum of 6 disks can be passed to disko."
    else builtins.length disks;
  nvme = rec {
    a = builtins.elemAt disks 0;
    b = if (numberOfDisks > 1)
          then builtins.elemAt disks 1
          else "";
    c = if (numberOfDisks > 2)
          then builtins.elemAt disks 2
          else "";
    d = if (numberOfDisks > 3)
          then builtins.elemAt disks 3
          else "";
    e = if (numberOfDisks > 4)
          then builtins.elemAt disks 4
          else "";
    f = if (numberOfDisks > 5)
          then builtins.elemAt disks 5
        else "";
    a1 = if (builtins.substring 0 9 a == "/dev/nvme")
         then "${a}p1"
         else "${a}-part1";
    a2 = if (builtins.substring 0 9 a == "/dev/nvme")
         then "${a}p2"
         else "${a}-part2";
    a3 = if (builtins.substring 0 9 a == "/dev/nvme")
         then "${a}p3"
         else "${a}-part3";
    b1 = if (builtins.substring 0 9 b == "/dev/nvme")
         then "${b}p1"
         else "${b}-part1";
    c1 = if (builtins.substring 0 9 c == "/dev/nvme")
         then "${c}p1"
         else "${c}-part1";
    d1 = if (builtins.substring 0 9 d == "/dev/nvme")
         then "${d}p1"
         else "${d}-part1";
    e1 = if (builtins.substring 0 9 e == "/dev/nvme")
         then "${e}p1"
         else "${e}-part1";
    f1 = if (builtins.substring 0 9 f == "/dev/nvme")
         then "${f}p1"
         else "${f}-part1";
  };
  raidPartitions =
    {
      "1" = "";
      "2" = "";
      "3" = "";
      "4" = "${nvme.c1}";
      "5" = "${nvme.c1} ${nvme.d1}";
      "6" = "${nvme.c1} ${nvme.d1} ${nvme.e1}";
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
              device = "${nvme.a1}";
              type = "EF00";
              start = "64M"; # A small mod8 offset, just because...
              end = "1G";
              content = {
                type = "filesystem";
                extraArgs = ["-n BOOT"];
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            swap = {
              label = "swap";
              device = "${nvme.a2}";
              size = "32G";
              content = {
                type = "swap";
                extraArgs = ["-L SWAP"];
                randomEncryption = true;
              };
            };
            root = {
              label = "root";
              device = "${nvme.a3}";
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = ["-L root"];
                subvolumes = {
                  "@" = {
                    mountpoint = "/";
                    mountOptions = ["subvol=@" "compress=zstd" "noatime"];
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
                device = "${nvme.b1}";
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
                device = "${nvme.f1}";
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
                    mountOptions = ["compress=zstd" "noatime" ];
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
                device = "${nvme.e1}";
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
                    mountOptions = ["compress=zstd" "noatime" ];
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
                device = "${nvme.d1}";
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
                device = "${nvme.c1}";
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
