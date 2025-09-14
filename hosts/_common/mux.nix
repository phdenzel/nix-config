{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    easytag
    exiftool
    flac2all
    handbrake
    kdePackages.kdenlive
    makemkv  # broken links
    mkvtoolnix
  ];
}
