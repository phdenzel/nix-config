{pkgs, ...}: {
  programs.aider-chat = {
    enable = true;
    package = pkgs.aider-chat-full.overrideAttrs (prev: {
      postInstall = (prev.postInstall or "") + ''
          wrapProgram $out/bin/aider --set OLLAMA_API_BASE http://127.0.0.1:11434
      '';
    });
    settings = {
      model = "ollama";
      architect = true;
      auto-commits = false;
      gitignore = false;
      attribute-committer = false;
      cache-prompts = true;
      dark-mode = true;
      pretty = true;
      lint = false;
    };
  };
}
