{...}: {
  imports = [
    ./profile.nix
    ./betterbird.nix
  ];
  programs.thunderbird = {
    enable = true;
  };
}
