{pkgs, ...}: {
  programs.aider-chat = {
    enable = true;
    package = pkgs.aider-chat-full.overrideAttrs (prev: {
      postInstall = (prev.postInstall or "") + ''
          wrapProgram $out/bin/aider --set OLLAMA_API_BASE http://127.0.0.1:11434
      '';
    });
    settings = {
      model = "ollama_chat/deepseek-r1:14b";
      editor-model = "ollama_chat/devstral-small-2:24b-instruct-2512-q4_K_M";
      architect = true;
      auto-commits = false;
      gitignore = false;
      attribute-committer = false;
      cache-prompts = true;
      thinking-tokens = 2048;
      dark-mode = true;
      pretty = true;
      lint = false;
    };
  };
}
