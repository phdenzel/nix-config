{...}: {
  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    compression = false;
    forwardAgent = true;
  };
}
