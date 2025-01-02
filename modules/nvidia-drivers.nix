{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.drivers.nvidia;
in {
  options.drivers.nvidia = {
    enable = mkEnableOption "Enable Nvidia Drivers";
    useOpen = mkOption {
      description = "Whether to use the open instead of proprietary drivers.";
      type = types.bool;
      default = true;
    };
    experimentalSupport = mkOption {
      description = "Whether to enable Experimental features support.";
      type = types.bool;
      default = true;
    };
  };

  config = mkIf cfg.enable {
    # Load nvidia driver for Xorg and wayland
    services.xserver.videoDrivers = mkDefault ["nvidia"];
    # OpenGL drivers
    hardware.graphics.enable = mkDefault true;
    # Nvidia drivers
    hardware.nvidia.modesetting.enable = mkDefault true;
    # Nvidia power management. May cause sleep/suspend to fail.
    hardware.nvidia.powerManagement.enable = mkDefault cfg.experimentalSupport;
    # Fine-grained power management. Turns off GPU when not in use.
    hardware.nvidia.powerManagement.finegrained = mkDefault cfg.experimentalSupport;
    # Use the nvidia open source kernel module (supported for Turing and newer architectures).
    # Full list of supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    hardware.nvidia.open = mkDefault cfg.useOpen;
    hardware.nvidia.nvidiaSettings = mkDefault true;
    hardware.nvidia.package = mkDefault config.boot.kernelPackages.nvidiaPackages.stable;
  };
}
