{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs;
    [
      signal-desktop
      slack
      zoom-us
    ]
    ++ optionals stdenv.isLinux [
      karere # whatsApp client alternative
      teams-for-linux
      webex # Linux-only (darwin: homebrew cask in hosts/_common/homebrew.nix)
    ]
    ++ optionals stdenv.isDarwin [
      whatsapp-for-mac
    ];
}
