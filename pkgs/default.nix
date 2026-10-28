# Custom nix packages
# You can build them using 'nix build .#example'
{pkgs, ...}: rec {
  comfy-cli = pkgs.callPackage ./comfy-cli {};
  comfy-mcp = pkgs.callPackage ./comfy-mcp {inherit comfy-cli;};
}
