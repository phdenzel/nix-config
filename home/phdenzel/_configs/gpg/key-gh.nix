{config, ...}: {
  programs.gpg = {
    publicKeys = [
      {
        source = "${config.home.homeDirectory}/.gnupg/gh.public.asc";
        trust = "ultimate";
      }
    ];
  };
}
