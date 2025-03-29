{pkgs, config, lib, ...}: let
  uwsmRun = program: "uwsm app -- ${program}";
  palette = config.colorScheme.palette;
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
      modules-right = ["tray" "idle_inhibitor" "group/hardware" "wireplumber" "bluetooth" "network" "battery" "custom/notification"];
      
      # Modules
      "battery" = {
        bat = "BAT0";
        adapter = "AC";
        design-capacity = false;
        format = "{icon} {capacity}%";
        format-alt = "{icon} {time}";
        format-charging = "󰂄 {capacity}%";
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
        format = " {num_connections}";
        format-disabled = "󰂴";
        format-off = "󰂲";
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
            months = "<span color='#${palette.subtext0}'><b>{}</b></span>";
            days = "<span color='#${palette.text}'><b>{}</b></span>";
            weeks = "<span color='#${palette.teal}'><b>W{}</b></span>";
            weekdays = "<span color='#${palette.sand}'><b>{}</b></span>";
            today = "<span color='#${palette.pink}'><b>{}</b></span>";
          };
          mode = "year";
          mode-mon-col = 3;
          on-scroll = 1;
          weeks-pos = "left";
        };
        format = "󱑎  {:%H:%M}";
        format-alt = "󰸗  {:%B %d, %Y}";
        timezone = "Europe/Zurich";
        tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
      };

      "cpu" = {
        format = " {usage}%";
        interval = 5;
        max-length = 6;
        on-click = "sleep 0.1 && pypr toggle monitor";
      };

      "custom/amd" = {
        exec = "rocm-smi -u --json | jq '.card0[] | tonumber'";
        format = "󱥒 {}%";
        interval = 5;
        max-length = 6;
        on-click = "sleep 0.1 && ${uwsmRun "lact gui"}";
        return-type = "";
        tooltip = false;
      };

      "custom/apps" = {
        format = "";
        on-click = "rofi -show drun -run-command \"${uwsmRun "{cmd}"}\"";
        tooltip = false;
      };

      "custom/clipboard" = {
        format = "";
        on-click = "sleep 0.1 && pypr toggle clipboard";
        tooltip = false;
      };

      "custom/lock" = {
        format = "";
        on-click = "${uwsmRun "hyprlock"}";
        tooltip = false;
      };

      "custom/logout" = {
        format = "󰍃";
        on-click = "hyprctl dispatch exit";
        tooltip = false;
      };

      "custom/nixos" = {
        format = "󱄅";
        on-click = "${uwsmRun "wlogout"}";
        tooltip = false;
      };

      "custom/notification" = {
        escape = true;
        exec = "swaync-client -swb";
        exec-if = "which swaync-client";
        format = "{icon} {}";
        format-icons = {
          notification = "󰂚<span foreground='#${palette.pink}'><sup></sup></span>";
          none = "󰂚";
          dnd-notification = "󰂛<span foreground='#${palette.pink}'><sup></sup></span>";
          dnd-none = "󰂛";
          inhibited-notification = "󰂜<span foreground='#${palette.pink}'><sup></sup></span>";
          inhibited-none = "󰂜";
          dnd-inhibited-notification = "󰪑<span foreground='#${palette.pink}'><sup></sup></span>";
          dnd-inhibited-none = "󰪑";  
        };
        on-click = "sleep 0.1 && ${uwsmRun "swaync-client -t -sw"}";
        on-click-right = "sleep 0.1 && ${uwsmRun "swaync-client -d -sw"}";
        return-type = "json";
        tooltip = false;
      };

      "custom/reboot" = {
        format = "";
        tooltip = false;
        on-click = "reboot";
      };

      "custom/turnoff" = {
        format = "";
        tooltip = false;
        on-click = "shutdown now";
      };

      "disk" = {
        format = " {free}";
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
          activated = "󱐌";
          deactivated = "";
        };
        tooltip = true;
      };

      "memory" = {
        format = " {percentage}%";
        interval = 5;
        max-length = 6;
        on-click = "sleep 0.1 && pypr toggle monitor";
      };

      "mpris" = {
        dynamic-len = 50;
        format = "{player_icon}  {status_icon} {dynamic}";
        # ignored-players = ["Lollypop"];
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
        format = "{ifname}";
        format-wifi = " {signalStrength}%";
        format-ethernet = "↑{bandwidthUpBytes} | ↓{bandwidthDownBytes}  󰈀  {ipaddr}";
        format-disconnected = "";
        tooltip-format = "󰈀 {ifname} via {gwaddri}";
        tooltip-format-wifi = " {essid} ({signalStrength}%)";
        tooltip-format-ethernet = " {ifname} ({ipaddr}/{cidr})";
        tooltip-format-disconnected = "Disconnected";
        max-length = 34;
        min-length = 34;
        on-click = "nm-connection-editor";
        on-click-right = "nmcli networking connectivity | grep -q none && nmcli networking on || nmcli networking off";
      };

      "power-profiles-daemon" = {
        format = "{icon}";
        tooltip = true;
        tooltip-format = "Power profile: {profile}\nDriver: {driver}";
        format-icons = {
          default = "";
          performance = "󰓅";
          balanced = "";
          power-saver = "";
        };
      };

      "tray" = {
        spacing = 10;
        icon-size = 18;
      };

      "wireplumber" = {
        format = "{icon} {volume}%";
        format-icons = ["" "" ""];
        format-muted = "";
        on-click = "pypr toggle volmgr";
      };

      "wlr/taskbar" = {
        format = " {icon}";
        icon-size = 18;
        ignore-list = ["^(scratchpad.*)$"];
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
          font-family: "Fira Sans SemiBold", FontAwesome, "DejaVu Sans", sans-serif;
          font-size: 16px;
      }

      window#waybar {
          background: alpha(#${palette.crust},.2);
          transition-property: background-property;
          transition-duration: .5s;
      }

      /* Group: Session */
      #custom-nixos {
          background: #${palette.overlay1};
          border-radius: 0px 0px 40px 0px;
          color: #${palette.subtext0};
          font-size: 28px;
          margin: 0px;
          padding: 0px 25px 0px 10px;
      }

      #custom-lock, #custom-logout, #custom-reboot, #custom-turnoff {
          background: #${palette.overlay1};
          font-size: 18px;
          margin: 0px;
          padding: 0px 8px;
      }

      #custom-nixos:hover, #custom-lock:hover, #custom-logout:hover, #custom-reboot:hover, #custom-turnoff:hover {
          background: #${palette.subtext0};
          color: #${palette.overlay1};
          opacity: 0.7;
      }

      /* Laucher */
      #custom-apps, #custom-clipboard {
          background: #${palette.overlay1};
          border-radius: 24px 8px 24px 8px;
          margin: 0px 4px;
          min-width: 30px;
          padding: 6px 6px;
      }

      /* Workspaces */
      #workspaces {
          background: #${palette.overlay1};
          border-radius: 24px 8px 24px 8px;
          color: #${palette.overlay1};
          margin: 0px 4px;
          opacity: 0.8;
          padding: 6px 6px;
      }

      #workspaces button {
          background-color: #${palette.subtext0};
          border-radius: 12px;
          color: #${palette.overlay1};
          margin: 0px 4px;
          opacity: 0.4;
          padding: 0px 6px;
          transition: all 0.3s ease-in-out;
      }

      #workspaces button.active {
          background: #${palette.subtext0};
          border-radius: 12px;
          color: #${palette.overlay1};
          min-width: 30px;
          opacity: 1.0;
          transition: all 0.3s ease-in-out;
      }

      #workspaces button:hover {
          background: #${palette.subtext0};
          border-radius: 12px;
          color: #${palette.overlay1};
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
          background: #${palette.overlay1};
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
        background: #${palette.overlay1};
        border-radius: 16px;
        color: #${palette.overlay1};
        opacity: 1.0;
        transition: all 0.3s ease-in-out;
      }

      #taskbar.empty {
        background-color: transparent;
        min-width: 0px;
        opacity: 0.0;
      }
      

      /* Tooltips */
      tooltip {
        background-color: #${palette.crust};
        border: 1px solid #${palette.overlay1};
        border-radius: 10px;
        opacity: 0.8;
      }
      tooltip label {
        color: #${palette.subtext0};
        font-family: "DejaVu Sans";
        font-size: 13px;
      }
    '';
  };
}
  
# /* color7 #EDF3FE;  -> textcolor2
#  * color8 #404850;  -> backgroundlight, workspacesbackground1, bordercolor, textcolorlightbg
#  * color15 #F6F9FE; -> backgrounddark, workspacesbackground2
#  * text    #BDC3CE; -> textcolor3, iconcolor */
