{...}: {
  programs.emacs.init.usePackage = {
    multiple-cursors.enable = true;
    sudo-edit.enable = true;
    comment-dwim-2.enable = true;
    hungry-delete.enable = true;
    expand-region.enable = true;
    drag-stuff.enable = true;
    company = {
      enable = true;
      hook = ["(after-init . global-company-mode)"];
      config = ''
        (setq company-idle-delay 0.3
              company-show-numbers t)
      '';
    };
    electric = {
      enable = true;
      command = [ "electric-indent-local-mode" ];
      config = ''
        (electric-pair-mode t)
      '';
      hook = [
        "(prog-mode . electric-indent-mode)"
        "(prog-mode . electric-layout-mode)"
      ];
    };
  };
}
