{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    texliveFull
    texinfo
    ghostscript
  ];
}
