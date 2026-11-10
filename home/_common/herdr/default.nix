{...}: {
  programs.herdr = {
    enable = true;
    settings = {
      onboarding = false;
      terminal = {
        default_shell = "zsh";
        new_cwd = "follow";
      };
      keys.prefix = "ctrl+z";
      ui = {
        toast.delivery = "herdr";
        sound.enabled = false;
      };
    };
  };
}
