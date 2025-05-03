{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    handbrake
    stable.makemkv
    mkvtoolnix
  ];
}
