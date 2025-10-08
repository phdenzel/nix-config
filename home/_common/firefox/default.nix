{pkgs, ...}: {
  imports = [./profile.nix];

  programs.browserpass = {
    enable = true;
    browsers = ["firefox"];
  };
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    languagePacks = ["en-US" "en-GB" "de"];
  };
}
