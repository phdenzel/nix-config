{pkgs, ...}: {
  home.packages = let
    default = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/phdenzel/cscs-sshservice-cli/refs/heads/main/default.nix";
      sha256 = "sha256:19jsp79n5w2nrpf805kgq44qcfwfbwn3jg6srpfa36w8ljrsqcni";
    };
    cscs-keygen = pkgs.callPackage default {
      src = pkgs.fetchFromGitHub {
        owner = "phdenzel";
        repo = "cscs-sshservice-cli";
        rev = "main";
        sha256 = "sha256-GSVj2KNBLeALhi/YZzJVylB19TQD32ihFbj9oO8Y+r4=";
      };
    };
  in
    [
      cscs-keygen
    ];
}
