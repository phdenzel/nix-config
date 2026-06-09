{pkgs, config, ...}: {
  imports = [./profile.nix];

  programs.browserpass = {
    enable = true;
    browsers = ["firefox"];
  };
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    configPath = "${config.xdg.configHome}/mozilla/firefox";
    languagePacks = ["en-US" "en-GB" "de"];
  };
}
