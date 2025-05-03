# This is the disko configuration for my work laptop
# From a single nmve drive, it creates a linux volume with
# 1) ESP partition
# 2) swap partition
# 3) nixos btrfs partition (subvol @, @home, @nix, @var, @snapshots)
# Devices in fenrix:
# - nixos:  nvme0n1 (/dev/nvme0n1)
{...}: let
  disks = ["/dev/nvme0n1"];
  nvme = rec {
    a = builtins.elemAt disks 0;
    a1 =
      if (builtins.substring 0 9 a == "/dev/nvme")
      then "${a}p1"
      else "${a}-part1";
    a2 =
      if (builtins.substring 0 9 a == "/dev/nvme")
      then "${a}p2"
      else "${a}-part2";
    a3 =
      if (builtins.substring 0 9 a == "/dev/nvme")
      then "${a}p3"
      else "${a}-part3";
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
              device = "${nvme.a1}";
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
              device = "${nvme.a2}";
              size = "32G";
              content = {
                type = "swap";
                extraArgs = ["-L swap"];
                # randomEncryption = true;
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
