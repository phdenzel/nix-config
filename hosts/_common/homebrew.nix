# Homebrew-managed apps for darwin hosts (apps without a nixpkgs darwin build).
# This module sets nix-darwin-only options, so import it ONLY from darwin hosts.
# Requires Homebrew itself to be installed on the system.
{...}: {
  homebrew = {
    enable = true;
    casks = ["alfred" "microsoft-teams" "webex"];
  };
}
