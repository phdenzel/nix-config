{pkgs, ...}: {
  imports = [./profile.nix];

  programs.browserpass.enable = true;
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    languagePacks = ["en-US" "en-GB" "de"];
  };
}
