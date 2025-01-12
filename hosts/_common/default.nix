# Common configuration for all hosts
{...}: {
  imports = [
    ./nix.nix
    ./sops.nix
    ./stylix.nix
  ];
}
