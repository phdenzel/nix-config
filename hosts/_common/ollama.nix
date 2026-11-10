{pkgs, lib, ...}: {
  services = {
    ollama.enable = true;
    ollama.package = lib.mkDefault pkgs.ollama;
    # AI models
    ollama.syncModels = true;
    ollama.loadModels = [
      "qwen3.6:35b-a3b-coding"
      "gemma4:12b"
      "qwen3.8:27b-q8_0"
      "qwen3-vl:30b-a3b-instruct"
      "embeddinggemma-2:740m"
    ];
    open-webui.enable = true;
    open-webui.package = pkgs.stable.open-webui;
    open-webui.port = 8080;
  };
}
