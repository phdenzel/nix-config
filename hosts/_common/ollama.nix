{pkgs, lib, ...}: {
  services = {
    ollama.enable = true;
    ollama.package = lib.mkDefault pkgs.ollama;
    # AI models
    ollama.syncModels = true;
    ollama.loadModels = import ./ollama-models.nix;
    open-webui.enable = true;
    open-webui.package = pkgs.stable.open-webui;
    open-webui.port = 8080;
  };
}
