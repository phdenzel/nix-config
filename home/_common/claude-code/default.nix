{...}: {
  programs.claude-code = {
    enable = true;
    mutableSettings = true;
    settings = {
      attribution = {
        commit = "";
        pr = "";
      };
      includeGitInstructions = false;
      effortLevel = "high";
      tui = "fullscreen";
      enabledPlugins = {
        "clangd-lsp@claude-plugins-official" = true;
        "rust-analyzer-lsp@claude-plugins-official" = true;
      };
    };
  };
}
