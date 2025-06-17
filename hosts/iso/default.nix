{modulesPath, ...}: {
  imports = [
    (modulesPath + "/profiles/base.nix")
    (modulesPath + "/profiles/all-hardware.nix")
    (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")
    (modulesPath + "/installer/cd-dvd/channel.nix")
    ./configuration.nix
  ];
}
