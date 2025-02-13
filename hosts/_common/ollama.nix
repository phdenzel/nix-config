{pkgs, ...}: {
  services = {
    ollama.enable = true;
    ollama.package = pkgs.stable.ollama;
    # AI models
    ollama.loadModels = [
      "llama3.1:8b-instruct-q8_0"
      "llama3.2:3b"
      "qwen2.5:3b"
      "qwen2.5-coder:3b"
      "qwen2.5-coder:7b-instruct-q8_0"
      "zephyr:7b-beta-q6_K"
      "deepseek-r1:7b"
      "mixtral:8x7b-instruct-v0.1-q3_K_M"
      "nomic-embed-text:v1.5"
    ];
    open-webui.enable = true;
    open-webui.package = pkgs.stable.open-webui;
    open-webui.port = 8080;
  };
}
