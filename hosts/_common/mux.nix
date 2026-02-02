{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    stable.easytag
    exiftool
    flac2all
    stable.handbrake
    stable.kdePackages.kdenlive
    makemkv  # broken links
    mkvtoolnix
  ];
}
