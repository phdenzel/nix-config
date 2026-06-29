{
  pkgs,
  lib,
  ...
}: {
  services.openssh =
    {
      enable = true;
      # disable RSA keys
      hostKeys = [
        {
          path = "/etc/ssh/ssh_host_ed25519_key";
          bits = 4096;
          type = "ed25519";
        }
      ];
    }
    // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
      # allow SSH root logins for remote builds
      settings.PermitRootLogin = lib.mkDefault "prohibit-password";
      allowSFTP = true;
    };
}
