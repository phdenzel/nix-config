# ComfyUI node-based diffusion server (linux-only; darwin: homebrew cask in hosts/_common/homebrew.nix)
# GPU acceleration follows the host's nixpkgs.config.rocmSupport / cudaSupport.
# Set services.comfyui.dataDir per host; members of the comfyui group get write access.
{
  config,
  lib,
  ...
}: let
  cfg = config.services.comfyui;
in {
  services.comfyui = {
    enable = true;
    port = 8188;
    listen = ["127.0.0.1"] ++ lib.optional config.networking.enableIPv6 "::1";
  };

  systemd.services.comfyui.serviceConfig = {
    UMask = "0007";
    StateDirectoryMode = lib.mkForce "2770";
  };

  systemd.tmpfiles.settings."10-comfyui".${cfg.dataDir}.d.mode = lib.mkIf (cfg.dataDir != "/var/lib/comfyui") (lib.mkForce "2770");
}
