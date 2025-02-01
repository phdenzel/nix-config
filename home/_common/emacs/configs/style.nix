{...}: {
  emacs.custom-theme.enable = true;
  programs.emacs.init.usePackage = {
    phd-ark-theme = {
      enable = true;
      init = ''
        (load-theme 'phd-ark :no-confirm)'';
      config = ''
        ;; (setq phd-ark-polarity 'dark)
        ;; (phd-ark/reload)
      '';
    };
  };
}
