# This is the disko configuration for my workstation
# It creates a
# - "nixos" volume with an ESP, swap, and a btrfs partition (subvol @, @home, @nix, @var, @snapshots)
# - "scratch" volume where miscellaneous games, containers, or VMs are stored
# - "raid" volume (btrfs raid with data as raid0 and metadata as raid1) where datasets are stored
# Device IDs in phinix:
# - linux:   CT4000T700SSD3_2341E87F4711
# - scratch: WD_BLACK_SN850X_4000GB_23322S800994
# - raid:    Samsung_SSD_990_PRO_4TB_S7DPNJ0X135359K -> eui.0025384141430d8c
# - raid:    Samsung_SSD_990_PRO_4TB_S7DPNJ0X135355N -> eui.0025384141430d88
# - raid:    Samsung_SSD_990_PRO_4TB_S7DPNJ0X135331M -> eui.0025384141430d07
# - raid:    Samsung_SSD_990_PRO_4TB_S7DPNU0X405324H -> eui.0025384441a17c56
{
  disks ? [
    "/dev/disk/by-id/nvme-CT4000T700SSD3_2341E87F4711"
    "/dev/disk/by-id/nvme-WD_BLACK_SN850X_4000GB_23322S800994"
    "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135359K"
    "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135355N"
    "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNJ0X135331M"
    "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_4TB_S7DPNU0X405324H"
  ],
  ...
}: let
  number_of_disks =
    if (builtins.length disks > 6)
    then builtins.length disks
    else throw "Error. For this scheme a maximum of 6 disks can be passed to disko.";
  nvme0 = builtins.elemAt disks 0;
  nvme1 =
    if (number_of_disks >= 1)
    then builtins.elemAt disks 1
    else "";
  nvme2 =
    if (number_of_disks >= 2)
    then builtins.elemAt disks 2
    else "";
  nvme3 =
    if (number_of_disks >= 3)
    then builtins.elemAt disks 3
    else "";
  nvme4 =
    if (number_of_disks >= 4)
    then builtins.elemAt disks 4
    else "";
  nvme5 =
    if (number_of_disks >= 5)
    then builtins.elemAt disks 5
    else "";
  nvme6 =
    if (number_of_disks == 6)
    then builtins.elemAt disks 6
    else "";
in {
  disko.devices = {
    disk = {
      nixos = {
        type = "disk";
        device = "${nvme0}";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              name = "ESP";
              type = "EF00";
              start = "64M"; # A small mod8 offset, just because...
              end = "1G";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            swap = {
              name = "swap";
              start = "1G";
              end = "65G";
              content = {
                type = "swap";
                randomEncryption = true;
              };
            };
            root = {
              name = "root";
              size = "100%";
              content = {
                type = "btrfs";
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
        if (number_of_disks < 2)
        then {}
        else {
          type = "disk";
          device = "${nvme1}";
          content = {
            type = "gpt";
            partitions = {
              empty = {
                size = "4G";
              };
              scratch = {
                name = "scratch";
                type = "btrfs";
                size = "100%";
                subvolumes = {
                  "@scratch" = {
                    mountpoint = "/scratch";
                    mountOptions = ["compress=zstd" "noatime"];
                  };
                };
              };
            };
          };
        };

      raid =
        if (number_of_disks < 3)
        then {}
        else {
          type = "disk";
          device = "${nvme2}";
          content = {
            type = "gpt";
            partitions = {
              raid = {
                name = "raid";
                type = "btrfs";
                size = "100%";
                extraArgs = [
                  "-d raid0"
                  "-m raid1"
                  "${nvme3}"
                  "${nvme4}"
                  "${nvme5}"
                  "${nvme6}"
                ];
                subvolumes = {
                  "@raid" = {
                    mountpoint = "/raid";
                    mountOptions = ["compress=zstd" "noatime"];
                  };
                };
              };
            };
          };
        };
    };
  };
}
