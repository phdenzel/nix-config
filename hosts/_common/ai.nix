{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    claude-code
    graphify
    # librechat
    # whisperx
  ];
}
