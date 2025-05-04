{config, ...}: {
  programs.gpg = {
    publicKeys = [
      {
        source = "${config.home.homeDirectory}/.gnupg/ecc.public.asc";
        trust = "ultimate";
      }
    ];
  };
}
