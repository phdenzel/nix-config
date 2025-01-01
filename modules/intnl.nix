{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.intnl-en;
in {
  options.intnl-en = {
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
    # Time zone settings.
	  time.timeZone = {
      "de_CH" = "Europe/Zurich";
      "de_DE" = "Europe/Berlin";
      "en_US" = "America/New_York";
      "en_GB" = "Europe/London";
    }."${cfg.extraLocale}";
    # Internationalisation/Language settings.
    i18n.defaultLocale = cfg.defaultLocale;
    i18n.extraLocaleSettings = {
	    LC_ADDRESS = "${cfg.extraLocale}.UTF-8";
      LC_COLLATE = "C.UTF-8";
      # LC_CTYPE = "C.UTF-8";
	    LC_IDENTIFICATION = "${cfg.extraLocale}.UTF-8";
	    LC_MEASUREMENT = "${cfg.extraLocale}.UTF-8";
      # LC_MESSAGES = "en_US.UTF-8";
	    LC_MONETARY = "${cfg.extraLocale}.UTF-8";
	    LC_NAME = "${cfg.extraLocale}.UTF-8";
	    LC_NUMERIC = "${cfg.extraLocale}.UTF-8";
	    LC_PAPER = "${cfg.extraLocale}.UTF-8";
	    LC_TELEPHONE = "${cfg.extraLocale}.UTF-8";
	    LC_TIME = "${cfg.extraLocale}.UTF-8";
	  };
  };
}
