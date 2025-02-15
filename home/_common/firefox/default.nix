{pkgs, ...}: {
  imports = [./profile.nix];

  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    languagePacks = ["en-US" "en-GB" "de"];
  };
}
