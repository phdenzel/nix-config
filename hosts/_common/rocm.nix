# AMD ROCm compute runtime (linux-only)
{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    rocmPackages.clr
  ];
}
