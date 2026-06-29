{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs;
    [
      age
      cacert
      openssl
      pwgen
      qrencode
      ssh-to-age
      sops
      zbar
    ]
    ++ optionals stdenv.hostPlatform.isLinux [
      merecat # for htpasswd
    ];

  programs.gnupg.agent = {
    enable = mkDefault true;
    enableSSHSupport = mkDefault true;
  };
}
