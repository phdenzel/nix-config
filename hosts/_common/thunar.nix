{pkgs, ...}: {
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-media-tags-plugin
      thunar-volman
    ];
  };
  environment.systemPackages = with pkgs; [
    unp
    xarchiver
  ];
  programs.xfconf.enable = true;
  services.tumbler.enable = true;
}
