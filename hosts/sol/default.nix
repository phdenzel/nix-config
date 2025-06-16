{inputs, ...}: {
  imports = [
    inputs.disko.nixosModules.disko
    ./disk-config.nix
    # ./hardware-configuration.nix
    ./configuration.nix
    ./users.nix
  ];
}
