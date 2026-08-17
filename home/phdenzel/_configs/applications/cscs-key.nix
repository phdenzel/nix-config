{
  inputs,
  system,
  ...
}: {
  home.packages = [
    inputs.cscs-key.packages.${system}.default
  ];
}
