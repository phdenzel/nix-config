{lib, ...}: {
  imports = [
    ./gpg.nix
    ./key-pwds.nix
    ./key-gh.nix
    ./key-ecc.nix
    ./key-pm.nix
    ./agent.nix
  ];
  programs.gpg = {
    settings.default-key = lib.mkDefault "629FC7317EFB4935";
  };
}
