{
  lib,
  config,
  ...
}:
with lib;
let
  cfg = config.drivers.nvidia-prime;
in
{
  options.drivers.nvidia-prime = {
    enable = mkEnableOption "Enable Nvidia Prime Hybrid GPU Offload";
    intelBusID = mkOption {
      type = types.str;
      default = "PCI:1:0:0";
    };
    nvidiaBusID = mkOption {
      type = types.str;
      default = "PCI:0:2:0";
    };
  };

  config = mkIf cfg.enable {
    hardware.nvidia.prime.offload.enable = true;
    hardware.nvidia.prime.offload.enableOffloadCmd = true;
    # Make sure to use the correct Bus ID values for your system!
    hardware.nvidia.prime.intelBusId = "${cfg.intelBusID}";
    hardware.nvidia.prime.nvidiaBusId = "${cfg.nvidiaBusID}";
  };
}

