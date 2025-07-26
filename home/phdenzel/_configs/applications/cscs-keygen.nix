{pkgs, ...}: {
  home.packages = let
    default = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/phdenzel/cscs-sshservice-cli/refs/heads/main/default.nix";
      sha256 = "sha256:0mqwn1lb1770ph1ls3l8axzrnhc1d11bldr32423ffdf7fww5wyd";
    };
    cscs-keygen = pkgs.callPackage default {
      src = pkgs.fetchFromGitHub {
        owner = "phdenzel";
        repo = "cscs-sshservice-cli";
        rev = "main";
        sha256 = "sha256-M3wOxYDj841mlZBI0b3+0udGDdlZhHo1Mec8vqBWsao=";
      };
    };
  in
    [
      cscs-keygen
    ];
}
