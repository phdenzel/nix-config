{pkgs, ...}: {
  home.packages = let
    default = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/phdenzel/cscs-sshservice-cli/refs/heads/main/default.nix";
      sha256 = "sha256:1lakwmn80ab2m3c43z1rwj4v5gk2a18hfma7ik9n2kqx1zrn75bq";
    };
    cscs-keygen = pkgs.callPackage default {
      src = pkgs.fetchFromGitHub {
        owner = "phdenzel";
        repo = "cscs-sshservice-cli";
        rev = "main";
        sha256 = "sha256-gbQh646rT/acFvsdxqqFwAuh3qyUvRylnjh8GbahTp4=";
      };
    };
  in
    [
      cscs-keygen
    ];
}
