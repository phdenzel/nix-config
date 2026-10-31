{pkgs, config, lib, ...}: let
  lock = "${pkgs.systemd}/bin/loginctl lock-session";
  dpms = action: "hyprctl dispatch 'hl.dsp.dpms({ action = \"${action}\" })'";
in {
  services.hypridle = {
    enable = true;
    settings = {
      general = {
        lock_cmd = "pgrep hyprlock || ${lib.getExe config.programs.hyprlock.package}";
        after_sleep_cmd = dpms "on";
        before_sleep_cmd = "loginctl lock-session";
      };

      listener = [
        {
          timeout = 300;
          on-timeout = "pidof hyprlock && ${dpms "off"}";
          on-resume = dpms "on";
        }
        {
          timeout = 480;
          on-timeout = "hyprctl hyprsunset gamma 30";
          on-resume = "hyprctl hyprsunset gamma 100";
        }
        {
          timeout = 600;
          on-timeout = lock;
        }
        {
          timeout = 900;
          on-timeout = dpms "off";
          on-resume = dpms "on";
        }
        {
          timeout = 1800;
          on-timeout = "systemctl suspend";
        }
      ];
    };
  };
}
