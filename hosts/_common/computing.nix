{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    ollama
  ];
  services = {
    jupyterhub.enable = true;
    jupyterhub.port = 8000;
    ollama.enable = true;
    open-webui.enable = true;
    open-webui.package = pkgs.stable.open-webui;
    open-webui.port = 8080;
  };
}
