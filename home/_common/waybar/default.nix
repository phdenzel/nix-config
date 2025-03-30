{pkgs, config, lib, ...}: let
  uwsmRun = program: "uwsm app -- ${program}";
  palette = config.colorScheme.palette;
  baseColor = palette.crust;
  bgColor = palette.overlay1;
  fgColor = palette.text;
  fgHColor = palette.subtext0;
in {
  home.packages = with pkgs; [nixos-icons];

  programs.waybar = {
    enable = true;
    systemd.enable = false;
    settings = lib.mkDefault [{
      # General
      layer = "top";
      height = 32;
      margin-top = 0;
      margin-bottom = 0;
      margin-left = 0;
      margin-right = 0;
      spacing = 1;

      # Build
      modules-left = ["group/session" "custom/apps" "custom/clipboard" "hyprland/workspaces" "wlr/taskbar" "mpris"];
      modules-center = ["clock"];
      modules-right = ["tray" "power-profiles-daemon" "group/hardware" "network" "bluetooth" "battery" "wireplumber" "custom/notification" "idle_inhibitor"];
      
      # Modules
      "battery" = {
        bat = "BAT0";
        adapter = "AC";
        design-capacity = false;
        format = "<span color='#${palette.viridis}'>{icon}</span> {capacity}%";
        format-alt = "<span color='#${palette.viridis}'>{icon}</span> {time}";
        format-charging = "<span color='#${palette.viridis}'>󰂄</span> {capacity}%";
        format-icons = ["󰂎" "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
        format-plugged = "󰚥 {capacity}%";
        format-time = "{H}h {M}m";
        full-at = 100;
        states = {
          critical = 15;
          good = 95;
          warning = 30;
        };
        tooltip = true;
      };

      "bluetooth" = {
        format = "<span color='#${palette.blue}'></span> {num_connections}";
        format-disabled = "<span color='#${palette.blue}'>󰂴</span>";
        format-off = "<span color='#${palette.blue}'>󰂲</span>";
        interval = 15;
        on-click = "blueman-manager";
        on-click-right = "bluetoothctl show | grep 'Powered: no' -q && bluetoothctl power on || bluetoothctl power off";
        tooltip-format = "{controller_alias}\t{controller_address}\n\n{num_connections} connected";
        tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{num_connections} connected\n\n{device_enumerate}";
	      tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
	      tooltip-format-enumerate-connected-battery = "{device_alias}\t{device_address}\t{device_battery_percentage}%";
      };

      "clock" = {
        actions = {
          on-click-right = "mode";
          on-scroll-up = "shift_up";
          on-scroll-down = "shift_down";
        };
        calendar = {
          format = {
            months = "<span color='#${fgHColor}'><b>{}</b></span>";
            days = "<span color='#${fgColor}'><b>{}</b></span>";
            weeks = "<span color='#${palette.teal}'><b>W{}</b></span>";
            weekdays = "<span color='#${palette.sand}'><b>{}</b></span>";
            today = "<span color='#${palette.pink}'><b>{}</b></span>";
          };
          mode = "year";
          mode-mon-col = 3;
          on-scroll = 1;
          weeks-pos = "left";
        };
        format = "<span color='#${palette.white}'>󱑎</span>  {:%H:%M}";
        format-alt = "<span color='#${palette.white}'>󰸗</span>  {:%B %d, %Y}";
        timezone = "Europe/Zurich";
        tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
      };

      "cpu" = {
        format = "<span color='#${palette.blue}'></span>  {usage}%";
        interval = 3;
        min-length = 5;
        max-length = 7;
        on-click = "sleep 0.1 && pypr toggle monitor";
      };

      "custom/nvidia" = {
        exec = "nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits";
        format = "<span color='#${palette.viridis}'>{icon}</span>  {}%";
        format-icons = "󰆧";
        interval = 3;
        min-length = 5;
        max-length = 7;
        on-click = "sleep 0.1 && ${uwsmRun "nvidia-settings"}";
        return-type = "";
        tooltip = true;
        tooltip-format = "GPU usage: {}%";
      };

      "custom/amd" = {
        exec = "rocm-smi -u --json | jq '.card0[] | tonumber'";
        format = "<span color='#${palette.pink}'>{icon}</span>  {}%";
        format-icons = "󱥒";
        interval = 3;
        min-length = 5;
        max-length = 7;
        on-click = "sleep 0.1 && ${uwsmRun "lact gui"}";
        return-type = "";
        tooltip = true;
        tooltip-format = "GPU usage: {}%";
      };

      "custom/apps" = {
        format = "<span color='#${palette.sand}'></span>";
        on-click = "rofi -show drun -run-command \"${uwsmRun "{cmd}"}\"";
        tooltip = true;
        tooltip-format = "Application launcher";
      };

      "custom/clipboard" = {
        format = "<span color='#${palette.purple}'></span>";
        on-click = "sleep 0.1 && pypr toggle clipboard";
        tooltip = true;
        tooltip-format = "Clipboard history";
      };

      "custom/lock" = {
        format = "";
        on-click = "${uwsmRun "hyprlock"}";
        tooltip = true;
        tooltip-format = "Lock current session";
      };

      "custom/logout" = {
        format = "󰍃";
        on-click = "hyprctl dispatch exit";
        tooltip = true;
        tooltip-format = "Logout of current session";
      };

      "custom/nixos" = {
        format = "<span color='#${palette.blue}'>󱄅</span>";
        on-click = "${uwsmRun "wlogout"}";
        tooltip = true;
        tooltip-format = "Logout menu";
      };

      "custom/notification" = {
        escape = true;
        exec = "swaync-client -swb";
        exec-if = "which swaync-client";
        format = "{icon}  {}";
        format-icons = {
          notification = "<span color='#${palette.teal}'>󰂚</span><span foreground='#${palette.pink}'><sup></sup></span>";
          none = "<span color='#${palette.teal}'>󰂚</span>";
          dnd-notification = "<span color='#${palette.sand}'>󰂛</span><span foreground='#${palette.pink}'><sup></sup></span>";
          dnd-none = "<span color='#${palette.sand}'>󰂛</span>";
          inhibited-notification = "<span color='#${palette.teal}'>󰂜</span><span foreground='#${palette.pink}'><sup></sup></span>";
          inhibited-none = "<span color='#${palette.teal}'>󰂜</span>";
          dnd-inhibited-notification = "<span color='#${palette.pink}'>󰪑</span><span foreground='#${palette.pink}'><sup></sup></span>";
          dnd-inhibited-none = "<span color='#${palette.pink}'>󰪑</span>";
        };
        on-click = "sleep 0.1 && ${uwsmRun "swaync-client -t -sw"}";
        on-click-right = "sleep 0.1 && ${uwsmRun "swaync-client -d -sw"}";
        return-type = "json";
        tooltip = true;
        tooltip-format = "Notification centre";
      };

      "custom/reboot" = {
        format = "";
        on-click = "reboot";
        tooltip = true;
        tooltip-format = "Reboot machine";
      };

      "custom/turnoff" = {
        format = "";
        on-click = "shutdown now";
        tooltip = true;
        tooltip-format = "Shut down machine";
      };

      "disk" = {
        format = "<span color='#${palette.sand}'></span>  {free}";
        interval = 30;
        path = "/";
        on-click = "sleep 0.1 && pypr toggle monitor";
      };

      "group/hardware" = {
        orientation = "inherit";
        modules = [
          "cpu"
          "custom/amd"
          "memory"
          "disk"
        ];
      };

      "group/session" = {
        orientation = "inherit";
        modules = [
          "custom/nixos"
          "custom/lock"
          "custom/logout"
          "custom/reboot"
          "custom/turnoff"
        ];
        drawer = {
          transition-duration = 500;
          children-class = "not-nixos";
          transition-left-to-right = false;
        };
      };

      "hyprland/workspaces" = {
        active-only = false;
        all-outputs = true;
        format = "{}";
        format-icons = {
          urgent = "";
          active = "";
          default = "";
        };
        persistent-workspaces = {
          "*" = 5;
        };
      };

      "idle_inhibitor" = {
        format = "{icon}";
        format-icons = {
          activated = "<span color='#${palette.sand}'>󱐌</span>";
          deactivated = "<span color='#${palette.blue}'></span>";
        };
        start-activated = false;
        tooltip = true;
        tooltip-format-activated = "Idle inhibitor is {status}";
        tooltip-format-deactivated = "Idle inhibitor is {status}";
      };

      "memory" = {
        format = "<span color='#${palette.teal}'></span>   {percentage}%";
        interval = 3;
        min-length = 6;
        max-length = 8;
        on-click = "sleep 0.1 && pypr toggle monitor";
      };

      "mpris" = {
        dynamic-len = 50;
        dynamic-order = ["title" "artist" "position" "length"];
        dynamic-importance-order = ["title" "artist" "position" "length"];
        format = "{player_icon}  {status_icon} {dynamic}";
        # ignored-players = ["Lollypop"];
        interval = 1;
        player-icons = {
          default = "";
          firefox = "󰈹";
          brave = "󰪂";
        };
        status-icons = {
          playing = "";
		      paused = "";
          stopped = "";
	      };
      };

      "network" = {
        interval = 3;
        family = "ipv4";
        format = "{ifname}";
        format-wifi = "<span color='#${palette.purple}'></span>  {signalStrength}%";
        format-ethernet = "<span color='#${palette.teal}'>↑</span> {bandwidthUpBytes} | <span color='#${palette.sand}'>↓</span> {bandwidthDownBytes}  <span color='#${palette.purple}'>󰈀</span>  {ipaddr}";
        format-disconnected = "";
        tooltip-format = "󰈀 {ifname} via {gwaddri}";
        tooltip-format-wifi = "  {essid} ({signalStrength}%)";
        tooltip-format-ethernet = "  {ifname} ({ipaddr}/{cidr})";
        tooltip-format-disconnected = "Disconnected";
        max-length = 35;
        min-length = 35;
        on-click = "nm-connection-editor";
        on-click-right = "nmcli networking connectivity | grep -q none && nmcli networking on || nmcli networking off";
      };

      "power-profiles-daemon" = {
        format = "{icon}";
        tooltip = true;
        tooltip-format = "Power profile: {profile}\nDriver: {driver}";
        format-icons = {
          default = "<span color='#${palette.viridis}'></span>";
          performance = "<span color='#${palette.pink}'>󰓅</span>";
          balanced = "<span color='#${palette.sand}'></span>";
          power-saver = "<span color='#${palette.viridis}'></span>";
        };
      };

      "tray" = {
        spacing = 10;
        icon-size = 18;
      };

      "wireplumber" = {
        format = "{icon} {volume}%";
        format-icons = ["<span color='#${palette.white}'></span>" "<span color='#${palette.sand}'></span>" "<span color='#${palette.pink}'></span>"];
        format-muted = "";
        on-click = "pypr toggle volmgr";
      };

      "wlr/taskbar" = {
        format = " {icon}";
        icon-size = 18;
        ignore-list = ["scratchpad.term" "scratchpad.filemgr" "scratchpad.monitor" "scratchpad.clipboard"];
        on-click = "activate";
        on-click-middle = "close";
        tooltip-format = "{title}";
      };
    }];
    style = ''
      /* General */
      * {
          border: none;
          border-radius: 0px;
          font-family: "Fira Sans SemiBold", FontAwesome, "DejaVu Sans", Helvetica, Arial, sans-serif;
          font-size: 16px;
      }

      window#waybar {
          background: alpha(#${baseColor},.2);
          color: #${fgColor};
          transition-property: background-property;
          transition-duration: .5s;
      }

      /* Tooltips */
      tooltip {
          background-color: #${palette.crust};
          border: 1px solid #${bgColor};
          border-radius: 10px;
          opacity: 0.8;
      }
      tooltip label {
          color: #${fgHColor};
          font-family: "DejaVu Sans";
          font-size: 13px;
      }

      /* Group: Session */
      #custom-nixos {
          background: #${bgColor};
          border-radius: 0px 0px 40px 0px;
          color: #${fgHColor};
          font-size: 28px;
          margin: 0px;
          padding: 0px 25px 0px 10px;
      }

      #custom-lock, #custom-logout, #custom-reboot, #custom-turnoff {
          background: #${bgColor};
          font-size: 18px;
          margin: 0px;
          padding: 0px 8px;
      }

      #custom-nixos:hover, #custom-lock:hover, #custom-logout:hover, #custom-reboot:hover, #custom-turnoff:hover {
          background: alpha(#${fgHColor},.8);
          color: #${bgColor};
          opacity: 0.7;
      }

      /* Left standard modules */
      #custom-apps, #custom-clipboard, #mpris {
          background: #${bgColor};
          border-radius: 24px 8px 24px 8px;
          margin: 0px 4px;
          min-width: 30px;
          padding: 6px 9px;
      }

      /* Workspaces */
      #workspaces {
          background: #${bgColor};
          border-radius: 24px 8px 24px 8px;
          color: #${bgColor};
          margin: 0px 4px;
          opacity: 0.8;
          padding: 6px 6px;
      }

      #workspaces button {
          background: #${fgColor};
          border-radius: 12px;
          color: #${bgColor};
          margin: 0px 4px;
          opacity: 0.4;
          padding: 0px 6px;
          transition: all 0.3s ease-in-out;
      }

      #workspaces button.active {
          background: #${fgHColor};
          border-radius: 12px;
          color: #${bgColor};
          min-width: 30px;
          opacity: 1.0;
          transition: all 0.3s ease-in-out;
      }

      #workspaces button:hover {
          background: #${fgHColor};
          border-radius: 12px;
          color: #${bgColor};
          opacity: 0.7;
      }

      .modules-left > widget:first-child > #workspaces {
          margin-left: 0;
      }

      .modules-right > widget:last-child > #workspaces {
          margin-right: 0;
      }

      /* Taskbar */
      #taskbar {
          background: #${bgColor};
          border-radius: 24px 8px 24px 8px;
          margin: 0px 4px;
          opacity: 0.8;
          padding: 0px 3px;
          
      }
      #taskbar button {
          border-radius: 16px;
          margin: 0px 4px;
          opacity: 0.4;
          padding: 0px 3px;
          transition: all 0.3s ease-in-out;
      }
      #taskbar button.active {
          background: #${bgColor};
          border-radius: 16px;
          color: #${bgColor};
          opacity: 1.0;
          transition: all 0.3s ease-in-out;
      }

      #taskbar.empty {
          background-color: transparent;
          min-width: 0px;
          opacity: 0.0;
      }

      /* Clock */
      #clock {
          background: #${bgColor};
          border-radius: 8px 8px 24px 24px;
          color: #${fgHColor};
          margin: 0px 8px;
          padding: 0px 16px;
      }

      /* Tray */
      #tray {
          background: #${bgColor};
          border-radius: 8px 24px 8px 24px;
          margin: 0px 4px;
          padding: 0px 12px;
      }
      #tray > .passive {
          -gtk-icon-effect: dim;
      }
      #tray > .needs-attention {
          -gtk-icon-effect: highlight;
      }

      /* Group: Hardware */
      #cpu, #custom-amd, #memory, #disk {
          background: #${bgColor};
          margin: 0px 0px 0px 0px;
          min-width: 30px;
          padding: 6px 9px;
      }
      #cpu {
          border-radius: 8px 0px 0px 24px;
          margin: 0px 0px 0px 4px;
      }
      #disk {
          border-radius: 0px 24px 8px 0px;
          margin: 0px 4px 0px 0px;
      }

      /* Right standard modules */
      #power-profiles-daemon, #network, #bluetooth, #battery, #wireplumber, #custom-notification {
          background: #${bgColor};
          border-radius: 8px 24px 8px 24px;
          margin: 0px 4px;
          min-width: 30px;
          padding: 6px 9px;
      }

      /* Idle inhibitor */
      #idle_inhibitor {
          background: #${bgColor};
          border-radius: 0px 0px 0px 40px;
          color: #${fgHColor};
          font-size: 24px;
          margin: 0px;
          padding: 0px 10px 0px 25px;
      }

      /* Other */
      label:focus {
           background-color: #000000;
      }

      #backlight {
           background-color: #${palette.yellow};
      }
    '';
  };
}
