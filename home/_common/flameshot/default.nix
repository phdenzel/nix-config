{...}: {
  services.flameshot = {
    enable = true;
    settings = {
      General = {
        disabledTrayIcon = false;
        showStartupLaunchMessage = false;
        showDesktopNotification = true;
      };
    };
  };
}
