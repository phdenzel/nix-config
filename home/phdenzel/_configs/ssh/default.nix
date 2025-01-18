{...}: {
  imports = [
    ./config.nix
    ./hosts.nix
  ];
  services.ssh-agent.enable = true;
}
