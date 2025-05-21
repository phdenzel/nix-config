{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    cscs-keygen
  ];
  nixpkgs.config.packageOverrides = pkgs: {
    cscs-keygen =
      let
        default = builtins.fetchurl {
          url = "https://raw.githubusercontent.com/phdenzel/cscs-sshservice-cli/refs/heads/main/default.nix";
          sha256 = "";
        };
      in pkgs.callPackage default {
        src = pkgs.fetchFromGitHub {
          owner = "phdenzel";
          repo = "cscs-sshservice-cli";
          rev = "main";
          sha256 = "";
        };
      };
  };
}
