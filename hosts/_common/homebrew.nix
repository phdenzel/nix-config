# Homebrew-managed apps for darwin hosts (apps without a nixpkgs darwin build).
# This module sets nix-darwin-only options, so import it ONLY from darwin hosts.
# Requires Homebrew itself to be installed on the system.
{...}: {
  homebrew = {
    enable = true;
    # Cask definitions come from the live API, so Homebrew itself must keep up with them
    onActivation.autoUpdate = true;
    casks = ["alfred" "claude" "comfy" "gimp" "microsoft-teams" "webex"];
  };
}
