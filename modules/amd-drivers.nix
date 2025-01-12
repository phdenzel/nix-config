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
      default = false;
    };
    vlk.enable = mkEnableOption "Use AMD's open source Vulkan driver.";
    utils.install = mkOption {
      description = "Install AMD graphics utilities.";
      type = types.bool;
      default = true;
    };
    utils.packages = mkOption {
      description = "List of utility packages.";
      type = with types; listOf package;
      default = with pkgs; [
        stable.rocmPackages.rocm-smi
        stable.btop-rocm
        clinfo
        libva-utils
        vdpauinfo
      ];
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      # Load driver into kernel
      hardware.amdgpu.initrd.enable = mkDefault true;
      # Load driver for Xorg and wayland
      # services.xserver.videoDrivers = mkDefault ["amdgpu"];
      services.xserver.videoDrivers = mkDefault ["modesetting"];
      # Mesa drivers
      hardware.graphics.enable = mkDefault true;
      hardware.graphics.enable32Bit = mkDefault true;
      # HIP libraries
      systemd.tmpfiles.rules = with pkgs;
        mkDefault [
          "L+    /opt/rocm/hip   -    -    -     -    ${rocmPackages.clr}"
        ];
    })
    (mkIf cfg.vlk.enable {
      # Vulkan
      hardware.amdgpu.amdvlk.enable = mkDefault cfg.useVlk;
      hardware.amdgpu.amdvlk.support32Bit.enable = mkDefault cfg.useVlk;
      hardware.amdgpu.amdvlk.supportExperimental.enable = mkDefault cfg.useVlk && cfg.experimentalSupport;
      # hardware.amdgpu.amdvlk.settings = {
      #   AllowVkPipelineCachingToDisk = 1; # ~/.cache/AMD/VkCache
      #   ShaderCacheMode = 1;
      #   IFH = 0;
      #   EnableVmAlwaysValid = 1;
      #   IdleAfterSubmitGpuMask = 1;
      # };
      # environment.variables.AMD_VULKAN_ICD = mkIf forceVlk "RADV";
    })
    (mkIf cfg.utils.install {
      environment.systemPackages = cfg.utils.packages;
    })
  ];
}
