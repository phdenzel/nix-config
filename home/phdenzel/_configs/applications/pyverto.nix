{pkgs, ...}: {
  home.packages = let
    default = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/phdenzel/pyverto/refs/heads/main/default.nix";
      sha256 = "";
    };
    pyverto = pkgs.callPackage default {
      src = pkgs.fetchFromGitHub {
        owner = "phdenzel";
        repo = "puverto";
        rev = "main";
        sha256 = "";
      };
    };
  in
    [
      pyverto
    ];
}
