{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    easytag
    exiftool
    flac2all
    handbrake
    makemkv  # broken links
    mkvtoolnix
  ];
}
