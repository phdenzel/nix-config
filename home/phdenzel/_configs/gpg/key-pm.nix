{config, ...}: {
  programs.gpg = {
    publicKeys = [
      {
        source = "${config.home.homeDirectory}/.gnupg/pm.public.asc";
        trust = "ultimate";
      }
    ];
  };
}
