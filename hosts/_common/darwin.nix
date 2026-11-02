# Common configuration for all darwin hosts
{
  config,
  pkgs,
  lib,
  inputs,
  outputs,
  ...
}: {
  imports = [
    inputs.sops-nix.darwinModules.sops
    ./sops.nix
  ];

  nix = {
    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = ["root" "@wheel" "phdenzel"];
      download-buffer-size = 524288000;
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 5d";
    };
    optimise.automatic = true;

    package = pkgs.nix;
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
    overlays = builtins.attrValues outputs.overlays;
  };

  # nix-darwin's vim module still passes the deprecated `vam` to vim-full.customize
  programs.vim.package = lib.mkIf config.programs.vim.enable (lib.mkForce (pkgs.vim-full.customize {
    name = "vim";
    vimrcConfig.customRC = config.environment.etc."vimrc".text;
  }));

  # nixpkgs' claude-code wrapper defaults to the system CA store only, which fails OAuth on macOS
  environment.variables.CLAUDE_CODE_CERT_STORE = "bundled,system";
}
