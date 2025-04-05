{config, ...}: let
  palette = config.colorScheme.palette;
  bgColor = palette.crust;
  bgHColor = palette.overlay1;
  fgColor = palette.white;
  borderR = "50";
  hoverBorderR = "80";
  hoverMargin = "-80";
in {
  # call wlogout with flag -b 6
  programs.wlogout = {
    enable = true;
    layout = [
      {
        label = "lock";
        action = "hyprlock";
        text = "Lock";
        keybind = "l";
      }
      {
        label = "logout";
        action = "hyprctl dispatch exit";
        text = "Logout";
        keybind = "u";
      }
      {
        label = "suspend";
        action = "systemctl suspend";
        text = "Suspend";
        keybind = "u";
      }
      {
        label = "shutdown";
        action = "systemctl poweroff";
        text = "Shutdown";
        keybind = "s";
      }
      {
        label = "reboot";
        action = "systemctl reboot";
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
