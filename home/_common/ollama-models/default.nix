{lib, ...}:
with lib; {
  options.ollama-models = {
    light = mkEnableOption "the light ollama model set (hosts/_common/ollama-models-light.nix) for hosts with limited memory";
  };
}
