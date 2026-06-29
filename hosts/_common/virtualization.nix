{...}: {
  # Linux-only: nix-darwin has no `virtualisation.podman` option.
  # Import this module conditionally on Linux hosts.
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
}
