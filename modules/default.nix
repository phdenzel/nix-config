{
  drivers.amdgpu = import ./amd-drivers.nix;
  drivers.nvidia = import ./nvidia-drivers.nix;
  drivers.nvidia-prime = import ./nvidia-primedrivers.nix;
  intl = import ./intl.nix;
}
