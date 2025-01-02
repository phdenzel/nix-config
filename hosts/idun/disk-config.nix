# This is the disko configuration for my nix development VM
# From a single vda drive, it creates a linux volume with
# 1) ESP partition
# 2) swap partition
# 3) nixos btrfs partition (subvol @, @home, @nix, @var, @snapshots)
# Devices in idun:
# - nixos:   vda (/dev/vda)
{
  disks ? [
    "/dev/vda"
  ],
  ...
}: let
  numberOfDisks =
    if (builtins.length disks > 1)
    then throw "Error. For this scheme a maximum of 1 disk(s) can be passed to disko."
    else builtins.length disks;
  vdisk = builtins.elemAt disks 0;
  partitions =
    {
      esp = "/dev/vda1";
      swap = "/dev/vda2";
      root = "/dev/vda3";
    };
in {
  disko.devices = {
    disk = {
      nixos = {
        type = "disk";
        device = "${vdisk}";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              label = "EFI";
              device = "${partitions.esp}";
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
              device = "${partitions.swap}";
              size = "4G";
              content = {
                type = "swap";
                extraArgs = ["-L swap"];
                # randomEncryption = true;
              };
            };
            root = {
              label = "root";
              device = "${partitions.root}";
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
    };
  };
}
