{pkgs, config, lib, ...}: let
  lock = "${pkgs.systemd}/bin/loginctl lock-session";
in {
  services.hypridle = {
    enable = true;
    settings = {
      general = {
        lock_cmd = "pgrep hyprlock || ${lib.getExe config.programs.hyprlock.package}";
        after_sleep_cmd = "hyprctl dispatch dpms on";
        before_sleep_cmd = "loginctl lock-session";
      };

      listener = [
        {
          timeout = 720;
          on-timeout = "hyprctl hyprsunset gamma 30";
          on-resume = "hyprctl hyprsunset gamma 100";
        }
        {
          timeout = 900;
          on-timeout = lock;
        }
        {
          timeout = 1080;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
        # {
        #   timeout = 5400;
        #   on-timeout = "systemctl suspend";
        # }
      ];
    };
  };
}
