{...}: {
  services.immich = {
    enable = true;
    openFirewall = true;
    port = 2283;
    machine-learning.enable = false;
  };
}
