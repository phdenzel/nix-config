{...}: {
  programs.emacs.extraConfig = ''
    (setq user-full-name "Philipp Denzel")
    (setq user-main-address "phdenzel@gmail.com")
  '';
}
