{pkgs, ...}: {
  services.flameshot = {
    enable = true;
    package = (pkgs.flameshot.override {enableWlrSupport = true;});
    settings = {
      General = {
        disabledTrayIcon = false;
        showStartupLaunchMessage = false;
        showDesktopNotification = true;
      };
    };
  };
}
