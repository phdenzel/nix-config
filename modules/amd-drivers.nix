{
  lib,
  pkgs,
  config,
  ...
}:
with lib; let
  cfg = config.drivers.amdgpu;
in {
  options.drivers.amdgpu = {
    enable = mkEnableOption "Enable (modern) AMD Drivers";
    experimentalSupport = mkOption {
      description = "Whether to enable Experimental features support.";
      type = types.bool;
      default = true;
    };
    useVlk = mkOption {
      description = "Use AMD's open source Vulkan driver.";
      type = types.bool;
      default = true;
    };
    installUtils = mkOption {
      description = "Install AMD graphics utilities.";
      type = types.bool;
      default = true;
    };
  };

  config = mkIf cfg.enable {
    # Load driver into kernel
    hardware.amdgpu.initrd.enable = true;
    # Load driver for Xorg and wayland
    services.xserver.videoDrivers = ["amdgpu"];
    # HIP libraries
    systemd.tmpfiles.rules = with pkgs; [
      "L+    /opt/rocm/hip   -    -    -     -    ${rocmPackages.clr}"
    ];
    # Mesa drivers
    hardware.graphics.enable = true;
    hardware.graphics.enable32Bit = true;
    # Vulkan
    hardware.amdgpu.amdvlk.enable = cfg.useVlk;
    hardware.amdgpu.amdvlk.support32Bit.enable = cfg.useVlk;
    hardware.amdgpu.amdvlk.supportExperimental.enable = cfg.useVlk && cfg.experimentalSupport;
    # environment.variables.AMD_VULKAN_ICD = mkIf forceVlk "RADV";
    # GPU utils
    environment.systemPackages = with pkgs;
      mkIf cfg.installUtils
      [
        btop-rocm
        rocmPackages.rocm-smi
        clinfo
        libva-utils
        vdpauinfo
      ];
  };
}
