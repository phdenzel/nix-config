{config, ...}: {
  programs.thunderbird.profiles."${config.home.username}" = {
    isDefault = true;
    settings = with config.colorScheme.palette; {
      "browser.display.background_color" = "#${base}";
      "browser.display.foreground_color" = "#${text}";
      "browser.anchor_color" = "#${blue}";
      "browser.active_color" = "#${teal}";
      "mailnews.reply_on_top" = 1;
      "mail.identity.default.reply_on_top" = 1;
    };
  };
}
