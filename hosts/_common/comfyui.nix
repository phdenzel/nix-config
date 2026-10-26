# ComfyUI node-based diffusion server (linux-only; darwin: homebrew cask in hosts/_common/homebrew.nix)
# GPU acceleration follows the host's nixpkgs.config.rocmSupport / cudaSupport.
{
  config,
  lib,
  ...
}: {
  services.comfyui = {
    enable = true;
    port = 8188;
    listen = ["127.0.0.1"] ++ lib.optional config.networking.enableIPv6 "::1";
  };
}
