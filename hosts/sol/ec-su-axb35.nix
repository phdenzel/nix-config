{
  config,
  pkgs,
  ...
}: let
  ec-su-axb35 = config.boot.kernelPackages.callPackage ../../pkgs/ec-su-axb35 {};
  powerModeSync = pkgs.writeShellApplication {
    name = "ec-su-axb35-power-mode-sync";
    runtimeInputs = with pkgs; [power-profiles-daemon glib.bin];
    text = ''
      apply() {
        local mode
        case "$(powerprofilesctl get)" in
          power-saver) mode=quiet ;;
          performance) mode=performance ;;
          *) mode=balanced ;;
        esac
        echo "$mode" > /sys/class/ec_su_axb35/apu/power_mode
        echo "power_mode=$mode"
      }
      apply
      gdbus monitor --system --dest org.freedesktop.UPower.PowerProfiles \
        --object-path /org/freedesktop/UPower/PowerProfiles |
        while read -r line; do
          case "$line" in
            *ActiveProfile*) apply ;;
          esac
        done
    '';
  };
in {
  # The driver has no DMI check and writes raw EC registers, so it is only
  # loaded on this Sixunited AXB35-02 board (GMKtec EVO-X2).
  boot.extraModulePackages = [ec-su-axb35];
  boot.kernelModules = ["ec_su_axb35"];

  environment.systemPackages = [ec-su-axb35];

  services.power-profiles-daemon.enable = true;

  systemd.services.ec-su-axb35-power-mode = {
    description = "Sync EC APU power mode with power-profiles-daemon";
    after = ["power-profiles-daemon.service" "systemd-modules-load.service"];
    requires = ["power-profiles-daemon.service"];
    partOf = ["power-profiles-daemon.service"];
    wantedBy = ["power-profiles-daemon.service"];
    unitConfig.ConditionPathExists = "/sys/class/ec_su_axb35/apu/power_mode";
    serviceConfig = {
      ExecStart = "${powerModeSync}/bin/ec-su-axb35-power-mode-sync";
      Restart = "always";
      RestartSec = 5;
    };
  };
}
