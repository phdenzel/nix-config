{lib, ...}: {
  # Note:
  #   import secret keys manually with
  #   `gpg --import ~/.gnupg/secret.key.asc`
  #   (you might have to `gpgconf --kill gpg-agent` when problems arise)
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
