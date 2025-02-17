{config, lib, ...}: {
  imports = [./theme.nix];
  
  programs.bat = {
    enable = true;
    config = {
      theme = lib.mkOverride 99 "${config.colorScheme.slug}";
    };
  };
}
