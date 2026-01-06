{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    stable.easytag
    exiftool
    flac2all
    handbrake
    kdePackages.kdenlive
    makemkv  # broken links
    mkvtoolnix
  ];
}
