# ComfyUI node-based diffusion server (linux-only; darwin: homebrew cask in hosts/_common/homebrew.nix)
# GPU acceleration follows the host's nixpkgs.config.rocmSupport / cudaSupport.
{...}: {
  services.comfyui = {
    enable = true;
    port = 8188;
  };
}
