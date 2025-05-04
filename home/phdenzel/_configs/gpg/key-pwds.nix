{config, ...}: {
  programs.gpg = {
    publicKeys = [
      {
        source = "${config.home.homeDirectory}/.gnupg/pwds.public.asc";
        trust = "ultimate";
      }
    ];
  };
}
