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
      diminish = ["ivy-mode"];
      commands = ["ivy-mode"];
      custom = {
        ivy-use-virtual-buffers = true;
      };
      config = ''(ivy-mode 1)'';
    };
    ace-window.enable = true;
  };
}
