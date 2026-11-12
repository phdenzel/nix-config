# Ollama server, model sync and Open WebUI as launchd agents. Import on darwin hosts only.
{
  pkgs,
  lib,
  ...
}: let
  # The Homebrew app bundles the MLX runtime, which nixpkgs' darwin build lacks
  ollamaBin = "/Applications/Ollama.app/Contents/Resources/ollama";
  ollama = pkgs.writeShellScriptBin "ollama" ''
    exec ${ollamaBin} "$@"
  '';
  models = import ./ollama-models-light.nix;
  homeDir = "/Users/phdenzel";
  openWebui = pkgs.stable.open-webui;
  webuiPort = "8080";
  webuiDir = "${homeDir}/Library/Application Support/open-webui";

  # Pull the declared models and remove any others, like services.ollama.syncModels on NixOS
  syncModels = pkgs.writeShellScript "ollama-sync-models" ''
    until ${ollama}/bin/ollama list >/dev/null 2>&1; do sleep 2; done
    declared=" ${lib.concatStringsSep " " models} "
    for model in $declared; do
      ${ollama}/bin/ollama pull "$model"
    done
    ${ollama}/bin/ollama list | tail -n +2 | ${pkgs.gawk}/bin/awk '{print $1}' | while read -r model; do
      case "$declared" in
        *" $model "*) ;;
        *) ${ollama}/bin/ollama rm "$model" ;;
      esac
    done
  '';

  # Open WebUI writes its secret key to the working directory
  startWebui = pkgs.writeShellScript "open-webui-start" ''
    mkdir -p "${webuiDir}"
    cd "${webuiDir}"
    exec ${openWebui}/bin/open-webui serve --host 127.0.0.1 --port ${webuiPort}
  '';
in {
  homebrew.casks = ["ollama-app"];
  environment.systemPackages = [ollama];

  # Ollama API on localhost:11434
  launchd.user.agents.ollama.serviceConfig = {
    ProgramArguments = [ollamaBin "serve"];
    # OpenAI-compatible clients like pi can't set num_ctx per request
    EnvironmentVariables.OLLAMA_CONTEXT_LENGTH = "32768";
    RunAtLoad = true;
    KeepAlive = true;
    StandardOutPath = "${homeDir}/Library/Logs/ollama.log";
    StandardErrorPath = "${homeDir}/Library/Logs/ollama.log";
  };

  # Runs once per login and whenever the model list changes
  launchd.user.agents.ollama-sync-models.serviceConfig = {
    ProgramArguments = ["${syncModels}"];
    RunAtLoad = true;
    StandardOutPath = "${homeDir}/Library/Logs/ollama-sync-models.log";
    StandardErrorPath = "${homeDir}/Library/Logs/ollama-sync-models.log";
  };

  # Open WebUI on localhost:${webuiPort}, backed by the local ollama API
  launchd.user.agents.open-webui.serviceConfig = {
    ProgramArguments = ["${startWebui}"];
    EnvironmentVariables = {
      DATA_DIR = webuiDir;
      STATIC_DIR = "${webuiDir}/static";
      HF_HOME = "${webuiDir}/hf_home";
      SENTENCE_TRANSFORMERS_HOME = "${webuiDir}/transformers_home";
      OLLAMA_BASE_URL = "http://127.0.0.1:11434";
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
    };
    RunAtLoad = true;
    KeepAlive = true;
    StandardOutPath = "${homeDir}/Library/Logs/open-webui.log";
    StandardErrorPath = "${homeDir}/Library/Logs/open-webui.log";
  };
}
