{pkgs, ...}: {
  home.packages = let
    default = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/phdenzel/pyverto/refs/heads/main/default.nix";
      sha256 = "sha256:1m5xzxrpy5vg6ffxvvjwk8db9a9ljm0yg5awmvwqkffq3286sz9q";
    };
    pyverto = pkgs.callPackage default {
      src = pkgs.fetchFromGitHub {
        owner = "phdenzel";
        repo = "pyverto";
        rev = "main";
        sha256 = "sha256-OGjDJJrHdaySPf/Zlfva0IL3SzP43A1E3tOgJb4iW8A=";
      };
    };
  in
    [
      pyverto
    ];
}
