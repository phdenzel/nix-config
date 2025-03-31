{config, ...}: let
  palette = config.colorScheme.palette;
  bgColor = palette.crust;
  bgHColor = palette.overlay1;
  fgColor = palette.white;
  acColor = palette.teal;
  borderR = 24;
  hoverBorderR = 16;
  margin = 10;
  hoverMargin = 16;
in {
  programs.wlogout = {
    enable = true;
    layout = [
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
      {
        label = "suspend";
        action = "hyprlock && systemctl suspend";
        text = "Suspend";
        keybind = "u";
      }
      {
        label = "hibernate";
        action = "systemctl hibernate";
        text = "Hibernate";
        keybind = "h";
      }
      {
        label = "logout";
        action = "hyprctl dispatch exit";
        text = "Logout";
        keybind = "u";
      }
      {
        label = "lock";
        action = "hyprlock";
        text = "Lock";
        keybind = "l";
      }
    ];
    style = ''
      * {
          background-image: none;
          font-size: 24px;
      }

      window {
          background-color: transparent;
      }

      button {
          animation = gradient_f 20s ease-in infinite;
          background-color: #${bgColor};
          background-position: center;
          background-repeat: no-repeat;
          background-size: 20%;
          border: none;
          border-radius: 0px;
          border-width: 0px;
          box-shadow = none;
          color: #${fgColor};
          outline-style = none;
          text-shadow = none;
      }

      button:focus {
          background-color: #${bgHColor};
          background-size: 30%;
      }

      button:hover {
          animation: gradient_f 20s ease-in infinite;
          background-color: #${acColor};
          background-size: 40%;
          border-radius: ${hoverBorderR}px;
          transition: all 0.3s cubic-bezier(.55,0.0,.28,1.682);
      }

      #button:hover#lock {
          border-radius: ${hoverBorderR}px;
          margin: ${hoverMargin}px 0px ${hoverMargin}px ${margin}px;
      }
      #button:hover#reboot {
          border-radius: ${hoverBorderR}px;
          margin: ${hoverMargin}px ${margin}px ${hoverMargin}px 0px;
      }
      button:hover#logout, button:hover#suspend, button:hover#shutdown, button:hover#hibernate {
          border-radius: ${borderR}px;
          margin: ${hoverMargin}px 0px ${hoverMargin}px 0px;
      }

      #lock {
          background-image: image(url("$HOME/.config/wlogout/icons/lock.png"), url("/usr/share/wlogout/icons/lock.png"), url("/usr/local/share/wlogout/icons/lock.png"));
          border-radius: ${borderR}px 0px 0px ${borderR}px;
          margin: ${margin}px 0px ${margin}px ${margin}px;
      }
      #logout, #suspend, #shutdown, #hibernate {
          border-radius: 0px 0px 0px 0px;
          margin: ${margin}px 0px ${margin}px 0px;
      }
      #reboot {
          border-radius: 0px ${borderR}px ${borderR}px 0px;
          margin: ${margin}px ${margin}px ${margin}px 0px;
      }
      #logout {
          background-image: image(url("$HOME/.config/wlogout/icons/logout.png"), url("/usr/share/wlogout/icons/logout.png"), url("/usr/local/share/wlogout/icons/logout.png"));
      }

      #suspend {
          background-image: image(url("$HOME/.config/wlogout/icons/suspend.png"), url("/usr/share/wlogout/icons/suspend.png"), url("/usr/local/share/wlogout/icons/suspend.png"));
      }

      #shutdown {
          background-image: image(url("$HOME/.config/wlogout/icons/shutdown.png"), url("/usr/share/wlogout/icons/shutdown.png"), url("/usr/local/share/wlogout/icons/shutdown.png"));
      }

      #hibernate {
          background-image: image(url("$HOME/.config/wlogout/icons/hibernate.png"), url("/usr/share/wlogout/icons/hibernate.png"), url("/usr/local/share/wlogout/icons/hibernate.png"));
      }

      #reboot {
          background-image: image(url("$HOME/.config/wlogout/icons/reboot.png"), url("/usr/share/wlogout/icons/reboot.png"), url("/usr/local/share/wlogout/icons/reboot.png"));
      }
    '';
  };
}
