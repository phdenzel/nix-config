{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.intl;
  localeMap = {
    
  };
  locationsMap = {
    "de_CH" = "Europe/Zurich";
    "de_DE" = "Europe/Berlin";
    "en_US" = "America/New_York";
    "en_GB" = "Europe/London";
  };
in {
  options.intl = {
    defaultLocale = mkOption {
      description = "Default locale setting e.g. de_CH, de_DE, or en_US.";
      type = types.str;
      default = "en_US";
    };
    extraLocale = mkOption {
      description = "Extra locale setting for non-default e.g. de_CH, de_DE, or en_US.";
      type = types.str;
      default = "de_CH";
    };
  };

  config = {
    # Time zone settings
    time.timeZone =
      if hasAttr "${cfg.extraLocale}" locationsMap
      then mkDefault locationsMap."${cfg.extraLocale}"
      else mkDefault "Europe/London";

    # Internationalisation/Language settings
    i18n.defaultLocale = mkDefault "${cfg.defaultLocale}.UTF-8";
    i18n.extraLocaleSettings = {
      LC_ADDRESS = mkDefault "${cfg.extraLocale}.UTF-8";
      LC_COLLATE = mkDefault "C.UTF-8";
      # LC_CTYPE = mkDefault "C.UTF-8";
      LC_IDENTIFICATION = mkDefault "${cfg.extraLocale}.UTF-8";
      LC_MEASUREMENT = mkDefault "${cfg.extraLocale}.UTF-8";
      # LC_MESSAGES = mkDefault "en_US.UTF-8";
      LC_MONETARY = mkDefault "${cfg.extraLocale}.UTF-8";
      LC_NAME = mkDefault "${cfg.extraLocale}.UTF-8";
      LC_NUMERIC = mkDefault "${cfg.extraLocale}.UTF-8";
      LC_PAPER = mkDefault "${cfg.extraLocale}.UTF-8";
      LC_TELEPHONE = mkDefault "${cfg.extraLocale}.UTF-8";
      LC_TIME = mkDefault "${cfg.extraLocale}.UTF-8";
    };

    # Keymap
    services.xserver.xkb.layout = mkDefault (
      strings.concatStringsSep "," [
        "${substring 3 5 (strings.toLower cfg.defaultLocale)}"
        "${substring 3 5 (strings.toLower cfg.extraLocale)}"
      ]
    );
    services.xserver.xkb.variant = mkDefault (
      strings.concatStringsSep "," [
        (strings.optionalString (cfg.defaultLocale == "en_US") "altgr-intl")
        (strings.optionalString (cfg.extraLocale == "en_US") "altgr-intl")
      ]
    );
  };
}
