{
  pkgs,
  config,
  ...
}: {
  programs.pi-coding-agent = {
    enable = true;
    mutableSettings = true;
    extraPackages = with pkgs; [
      bun
      nodejs
    ];
    settings = {
      defaultThinkingLevel = "medium";
      theme = "system";
      quietStartup = "header";
      compaction = {
        enabled = true;
        reserveTokens = 16384;
        keepRecentTokens = 20000;
      };
    };
    models = {
      providers = {
        ollama = {
          baseUrl = "http://127.0.0.1:11434/v1";
          api = "openai-completions";
          apiKey = "ollama";
          models =
            if config.ollama-models.light
            then [
              {id = "qwen3.5:9b-q8_0";}
              {id = "gemma4:12b";}
              {id = "qwen3-vl:8b";}
            ]
            else [
              {id = "qwen3.6:35b-a3b-coding";}
              {id = "qwen3.8:27b-q8_0";}
              {id = "gemma4:12b";}
              {id = "qwen3-vl:30b-a3b-instruct";}
            ];
        };
      };
    };
  };
}
