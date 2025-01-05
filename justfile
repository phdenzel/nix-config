# Useful commands for building and deploying the nix-configs

# List all available commands
default:
    @just --list

# Show install commands
show-iso-cmds MACHINE="idun":
    echo "just disko "{{MACHINE}}
    echo "just iso-config"
    echo "just hardware-config"
    echo "just iso-install"

# Show install commands (extensive info)
show-iso-longcmds:
    for s in "disko" "iso-config" "hardware-config" "iso-install"; do just -s "$s"; done

# Dry-run the disko configuration (formatting and mounting) for specified machine.
test-disko MACHINE="idun":
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode format,mount --dry-run ./hosts/{{MACHINE}}/disk-config.nix

# Run the disko configuration (formatting and mounting) for specified machine.
disko MACHINE:
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode format,mount ./hosts/{{MACHINE}}/disk-config.nix

# Install minimal configuration.nix to /mnt/etc/nixos
iso-config:
    [ -d "/iso" ] && sudo mkdir -p /mnt/etc/nixos
    [ -d "/iso" ] && sudo cp iso/configuration.nix /mnt/etc/nixos/configuration.nix
    @just hardware-config

# Print a new hardware-configuration.nix file
show-hardware-config:
    [ -d "/iso" ] && sudo nixos-generate-config --root /mnt --show-hardware-config || sudo nixos-generate-config --show-hardware-config

# Generate a new hardware-configuration.nix file
hardware-config:
    [ -d "/iso" ] && sudo nixos-generate-config --root /mnt || sudo nixos-generate-config

# Install from iso
iso-install:
    [ -d "/mnt/boot" ] || just disko
    [ -f "/mnt/etc/nixos/configuration.nix" ] || just iso-config
    [ -f "/mnt/etc/nixos/hardware-configuration.nix" ] || just hardware-config
    [ -d "/iso" ] || nixos-install


ssh-to-age KEYFILE="/etc/ssh/ssh_host_ed25519_key.pub" MACHINE:
    # nix-shell -p ssh-to-age --run 'cat {{KEYFILE}} | ssh-to-age'
    AGE_KEY="$(nix-shell -p ssh-to-age --run 'cat {{KEYFILE}} | ssh-to-age')" && nix-shell -p yq-go --run "yq .keys[1][]+=[$AGE_KEY] | .keys[1][][-1] anchor = $MACHINE .sops.yaml"

# Rebuild switch shorthand
rbs MACHINE:
    sudo nixos-rebuild switch --flake .\#{{MACHINE}}
