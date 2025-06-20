{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    handbrake
    makemkv  # broken links
    mkvtoolnix
  ];
}
