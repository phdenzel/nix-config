# Useful commands for building and deploying the nix-configs

# List all available commands
default:
    @just --list

build IMG:
	nix build .#images.{{IMG}}

flash DEVICE IMG=shell('ls ./result/iso/nixos-*.iso'):
	sudo dd if={{IMG}} of={{DEVICE}} status=progress bs=4M conv=fsync

flash-sd DEVICE IMG=shell('ls ./result/sd-image/nixos-image-sd-card-*.img'):
	sudo dd if={{IMG}} of={{DEVICE}} status=progress bs=4M conv=fsync

# Show install commands
help-install MACHINE="idun":
    @echo "just disko {{MACHINE}}"
    @echo "just iso-install"

# Dry-run the disko configuration (formatting and mounting) for specified machine.
test-disko MACHINE="idun":
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode destroy,format,mount --dry-run ./hosts/{{MACHINE}}/disk-config.nix

# Step 1 for fresh install:
# Run the disko configuration (formatting and mounting) for specified machine.
disko MACHINE:
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode destroy,format,mount --yes-wipe-all-disks ./hosts/{{MACHINE}}/disk-config.nix

# Print a new hardware-configuration.nix file
show-hardware-config:
    [ -d "/iso" ] && sudo nixos-generate-config --root /mnt --show-hardware-config || sudo nixos-generate-config --show-hardware-config

# Step 2 for fresh install:
# Generate hardware configuration, optionally install flake dependecies, and install NixOS
iso-install MACHINE:
	#!/usr/bin/env sh
	[ -d "/mnt/boot" ] || just disko {{MACHINE}}
	sudo nixos-generate-config --kernel latest --no-filesystems --root /mnt
	if [ -d "/home/nixos/nix-config" ]; then
	    sudo mkdir -p /mnt/root
		sudo cp -r /home/nixos/nix-config /mnt/root/nix-config
		sudo install -o root -g root -m 644 /mnt/etc/nixos/hardware-configuration.nix /mnt/root/nix-config/hosts/{{MACHINE}}/
	fi
	if [ -d "/root/.config/sops" ]; then
	    sudo mkdir -p /mnt/root/.config/sops/age
		sudo cp -r /root/.config/sops/. /mnt/root/.config/sops/
		sudo chmod 700 /mnt/root/.config/sops
		sudo chmod 600 /mnt/root/.config/sops/age/keys.txt
	fi
	if [ -d "/home/nixos/nix-config" ]; then
	    sudo nixos-install --flake /mnt/root/nix-config#{{MACHINE}}
	else
	    if [ -f "/iso/local/etc/nixos/configuration.nix" ]; then
		    sudo mkdir -p /mnt/etc/nixos
			sudo cp /iso/local/etc/nixos/configuration.nix /mnt/etc/nixos/configuration.nix
		fi
		sudo nixos-install
	fi

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

gcn DAYS:
	sudo nix-collect-garbage --delete-older-than {{DAYS}}d
