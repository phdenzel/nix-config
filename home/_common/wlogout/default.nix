{pkgs, config, ...}: let
  sessionAction = pkgs.writeShellApplication {
    name = "session-action";
    runtimeInputs = with pkgs; [systemd libnotify];
    text = ''
      action="''${1:-}"
      run() {
        local out
        if ! out="$("$@" 2>&1)"; then
          printf '%s failed: %s\n' "$action" "$out" | systemd-cat -t session-action -p err
          notify-send -u critical "Session action failed: $action" "$out"
          exit 1
        fi
        printf '%s\n' "$action" | systemd-cat -t session-action
      }
      case "$action" in
        lock) run loginctl lock-session ;;
        logout) run systemd-run --user --collect --quiet "$(command -v uwsm)" stop ;;
        suspend)
          hyprctl dispatch 'hl.dsp.dpms({ action = "off" })' >/dev/null || :
          run systemctl suspend
          ;;
        reboot) run systemctl reboot ;;
        poweroff) run systemctl poweroff ;;
        *)
          echo "usage: session-action lock|logout|suspend|reboot|poweroff" >&2
          exit 2
          ;;
      esac
    '';
  };
  palette = config.colorScheme.palette;
  bgColor = palette.crust;
  bgHColor = palette.overlay1;
  fgColor = palette.white;
  borderR = "50";
  hoverBorderR = "80";
  hoverMargin = "-80";
in {
  home.packages = [sessionAction];

  # call wlogout with flag -b 6
  programs.wlogout = {
    enable = true;
    layout = [
      {
        label = "lock";
        action = "session-action lock";
        text = "Lock";
        keybind = "l";
      }
      {
        label = "logout";
        action = "session-action logout";
        text = "Logout";
        keybind = "e";
      }
      {
        label = "suspend";
        action = "session-action suspend";
        text = "Suspend";
        keybind = "u";
      }
      {
        label = "shutdown";
        action = "session-action poweroff";
        text = "Shutdown";
        keybind = "s";
      }
      {
        label = "reboot";
        action = "session-action reboot";
        text = "Reboot";
        keybind = "r";
      }
    ];
    style = ''
      * {
          background-image: none;
          font-size: 28px;
      }

      window {
          background-color: alpha(#${bgColor}, 0.2);
      }

      button {
          animation: gradient_f 20s ease-in infinite;
          background-color: #${bgColor};
          background-position: center;
          background-repeat: no-repeat;
          background-size: 10%;
          border: none;
          border-radius: 0px;
          border-width: 0px;
          box-shadow: none;
          color: #${fgColor};
          outline-style: none;
          text-shadow: none;
      }

      button:focus, button:hover {
          animation: gradient_f 20s ease-in infinite;
          background-color: alpha(#${bgHColor}, 0.7);
          background-size: 20%;
          border-radius: ${hoverBorderR}px ${hoverBorderR}px ${hoverBorderR}px ${hoverBorderR}px;
          margin: ${hoverMargin}px 0px ${hoverMargin}px 0px;
          transition: all 0.3s cubic-bezier(.55,0.0,.28,1.682);
      }

      button:focus#lock, button:focus#logout, button:focus#suspend, button:focus#shutdown, button:focus#hibernate, button:focus#reboot, button:hover#lock, button:hover#logout, button:hover#suspend, button:hover#shutdown, button:hover#hibernate, button:hover#reboot {
          border-radius: ${hoverBorderR}px ${hoverBorderR}px ${hoverBorderR}px ${hoverBorderR}px;
      }
      
      #lock {
          background-image: -gtk-icontheme('system-lock-screen-symbolic');
          border-radius: ${borderR}px 0px 0px ${borderR}px;
      }
      #logout {
          background-image: -gtk-icontheme('system-log-out-symbolic');
      }

      #suspend {
          background-image: -gtk-icontheme('weather-clear-night-symbolic');
      }

      #shutdown {
          background-image: -gtk-icontheme('system-shutdown-symbolic');
      }

      #reboot {
          background-image: -gtk-icontheme('system-reboot-symbolic');
          border-radius: 0px ${borderR}px ${borderR}px 0px;
      }
    '';
  };
}
