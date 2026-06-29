# Linux-only dev tools
# Import this module only on linux hosts.
{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    gdb
    perf # linux kernel profiling tool
  ];

  programs = {
    java.enable = true;
    java.binfmt = true;
  };
}
