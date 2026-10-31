{inputs, ...}: {
  imports = [
    inputs.disko.nixosModules.disko
    ./disk-config.nix
    ./hardware-configuration.nix
    ./ec-su-axb35.nix
    ./configuration.nix
    ./users.nix
  ];
}
