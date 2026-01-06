{pkgs, lib, ...}: {
  services = {
    ollama.enable = true;
    ollama.package = lib.mkDefault pkgs.ollama;
    # AI models
    ollama.loadModels = [
      "devstral-small-2:24b-instruct-2512-q4_K_M"
      "deepseek-coder-v2:16b"
      "qwen3-coder:30b"
      "qwen2.5-coder:32b-instruct"
      "codegemma:7b-instruct"
      "deepseek-r1:14b"
      "gpt-oss:20b"
      "gemma3:27b-it-q4_K_M"
      "llama3.2:3b"
      "nomic-embed-text:v1.5"
    ];
    open-webui.enable = true;
    open-webui.package = pkgs.stable.open-webui;
    open-webui.port = 8080;
  };
}
