# Common configuration for all NixOS hosts
{inputs, ...}: {
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./nix.nix
    ./sops.nix
    ./stylix.nix
  ];
}
