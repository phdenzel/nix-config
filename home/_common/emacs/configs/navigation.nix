{...}: {
  programs.emacs.init.usePackage = {
    dired = {
      enable = true;
      config = ''
        (put 'dired-find-alternate-file 'disabled nil)
        (setq delete-by-moving-to-trash t)
      '';
    };
    counsel.enable = true;
    swiper.enable = true;
    avy.enable = true;
    ivy = {
      enable = true;
      config = ''
        (setq ivy-use-virtual-buffers t)
      '';
    };
    ace-window.enable = true;
  };
}
