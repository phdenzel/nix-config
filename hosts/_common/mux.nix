{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    handbrake
    makemkv
    mkvtoolnix
  ];
}
