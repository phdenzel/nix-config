{lib, ...}: 
with lib; let
  cfg = config.emacs.phd-ark-theme;
in {
  options.emacs.phd-ark-theme = {
    enable = mkEnableOption "Enable the custom emacs theme.";
  };
  config = mkIf cfg.enable {
    
  };
}
