{...}: {
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
