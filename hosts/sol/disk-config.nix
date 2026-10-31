# This is the disko configuration for my workstation
# From a set of 1-2 NVME drives, it creates a
# 1) "nixos" volume with an ESP, swap, and a btrfs partition (subvol @, @home, @nix, @var, @snapshots)
# 2) "scratch" btrfs volume where miscellaneous games, containers, and VMs are stored (subvol @scratch, @games, @virt)
# Devices in sol:
# - nixos:    /dev/disk/by-id/nvme-WD_BLACK_SN850X_8000GB_25177P4A3G07
# - scratch:  /dev/disk/by-id/nvme-WD_BLACK_SN850X_8000GB_25177P4A4210
{...}: let
  disks = [
    "/dev/disk/by-id/nvme-WD_BLACK_SN850X_8000GB_25177P4A3G07"
    "/dev/disk/by-id/nvme-WD_BLACK_SN850X_8000GB_25177P4A4210"
  ];
  numberOfDisks =
    if (builtins.length disks > 2)
    then throw "Error. For this scheme a maximum of 2 disks can be passed to disko."
    else builtins.length disks;
  nvme = {
    a = builtins.elemAt disks 0;
    b =
      if (numberOfDisks > 1)
      then builtins.elemAt disks 1
      else "";
  };
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
                discardPolicy = "both";
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
                    "@data" = {
                      mountpoint = "/scratch/data";
                      mountOptions = ["subvol=@data" "compress=zstd" "noatime"];
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
    };
  };
}
