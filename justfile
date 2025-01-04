# Useful commands for building and deploying the nix-configs

# List all available commands
default:
    @just --list

# Dry-run the disko configuration (formatting and mounting) for specified machine.
test-disko MACHINE:
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode format,mount --dry-run ./hosts/{{MACHINE}}/disk-config.nix

# Run the disko configuration (formatting and mounting) for specified machine.
disko MACHINE:
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode format,mount ./hosts/{{MACHINE}}/disk-config.nix

# Print a new hardware-configuration.nix file
show-hardware-config:
    [ -d /iso ] && sudo nixos-generate-config --root /mnt --show-hardware-config || sudo nixos-generate-config --show-hardware-config

# Generate a new hardware-configuration.nix file
hardware-config:
    [ -d /iso ] && sudo nixos-generate-config --root /mnt --no-filesystems || sudo nixos-generate-config --no-filesystems

