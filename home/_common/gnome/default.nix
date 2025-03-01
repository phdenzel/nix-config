{lib, ...}: {
  dconf.settings = {
    "org/gnome/desktop/peripherals/keyboard" = {
      delay = lib.hm.gvariant.mkUint32 180;
      repeat-interval = lib.hm.gvariant.mkUint32 25;
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      terminal = "disabled";
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
      ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      name = "terminal";
      command = "ghostty";
      binding = "<Super>Return";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
      name = "rofi";
      command = "rofi -show run -display-run 'run: '";
      binding = "<Super>/";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
      name = "emacs";
      command = "emacsclient -c";
      binding = "<Super>e";
    };
  };
}
