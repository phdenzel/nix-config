# Useful commands for building and deploying the nix-configs

# List all available commands
default:
    @just --list

build IMG:
	nix build .#images.{{IMG}}

flash DEVICE IMG=shell('ls ./result/iso/nixos-*.iso'):
	sudo dd if={{IMG}} of={{DEVICE}} status=progress bs=4M

flash-sd DEVICE IMG=shell('ls ./result/sd-image/nixos-image-sd-card-*.img'):
	sudo dd if={{IMG}} of={{DEVICE}} status=progress bs=4M

# Show install commands
show-iso-cmds MACHINE="idun":
    @echo "just disko {{MACHINE}}"
    @echo "just iso-config"
    @echo "just iso-install"

# Show install commands (extensive info)
show-iso-longcmds:
    for s in "disko" "iso-config" "iso-install"; do just -s "$s"; done

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
iso-install MACHINE:
    [ -d "/mnt/boot" ] || just disko {{MACHINE}}
    [ -f "/mnt/etc/nixos/configuration.nix" ] || just iso-config
    [ -f "/mnt/etc/nixos/hardware-configuration.nix" ] || just hardware-config
    [ -d "/iso" ] && sudo nixos-install

# Append a host age key to the .sops.yaml file
host-age-key HOST KEYFILE="/etc/ssh/ssh_host_ed25519_key.pub":
    AGE_KEY="$(cat {{KEYFILE}} | ssh-to-age)" && yq -i ".keys[1][]+=[\"$AGE_KEY\"] | .keys[1][][-1] anchor = \"{{HOST}}\"" .sops.yaml
    @just update-secrets

# Update secrets.yaml files with new authorized keys
update-secrets:
    [ -f "${HOME}/.config/sops/age/keys.txt" ] && sops updatekeys hosts/secrets.yaml && sops updatekeys home/phdenzel/secrets.yaml  || echo "Install authorized AGE keys and run `just update-secrets` again."

# Dump pizauth
spiz:
	pizauth dump | age --encrypt --output pizauth.age -r age10eyh3zd3kusensxuy6j0g82x3qdjguju9r7ryk35zyl67j8w9gxqgx0aqp
	mv pizauth.age ~/.config/

# Restore pizauth
repiz:
	age --decrypt -i ~/.config/sops/age/keys.txt -o - ~/.config/pizauth.age | pizauth restore

# Rebuild switch shorthand
rbs MACHINE:
    sudo nixos-rebuild switch --show-trace --flake .#{{MACHINE}}

rep:
	sudo nix-store --repair --verify --check-contents

gc:
	sudo nix-collect-garbage -d

gcn AGE:
	sudo nix-collect-garbage --delete-older-than {{AGE}}d
