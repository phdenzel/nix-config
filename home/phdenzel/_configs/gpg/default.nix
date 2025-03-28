{
  config,
  pkgs,
  ...
}: {
  programs.gpg = {
    enable = true;
    settings = {
      default-key = "629FC7317EFB4935";
    };
    publicKeys = [
      {
        source = "${config.home.homeDirectory}/.gnupg/pwds.public.asc";
        trust = "ultimate";
      }
      {
        source = "${config.home.homeDirectory}/.gnupg/gh.public.asc";
        trust = "ultimate";
      }
      {
        source = "${config.home.homeDirectory}/.gnupg/ecc.public.asc";
        trust = "ultimate";
      }
      {
        source = "${config.home.homeDirectory}/.gnupg/pm.public.asc";
        trust = "ultimate";
      }
    ];
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 64800;
    maxCacheTtl = 64800;
    extraConfig = ''
      no-allow-external-cache
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };
}
