# Useful commands for building and deploying the nix-configs

# List all available commands
default:
    @just --list

purge-disks:
    sudo nvme format --force /dev/nvme0n1
    sudo nvme format --force /dev/nvme1n1
    sudo nvme format --force /dev/nvme2n1
    sudo nvme format --force /dev/nvme3n1
    sudo nvme format --force /dev/nvme4n1
    sudo nvme format --force /dev/nvme5n1
    sudo partprobe -s /dev/nvme*n1
    

# Dry-run the disko configuration (formatting and mounting) for specified machine.
disko-test MACHINE:
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode format,mount --dry-run ./hosts/{{MACHINE}}/disk-config.nix

# Run the disko configuration (formatting and mounting) for specified machine.
disko MACHINE:
    sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode format,mount ./hosts/{{MACHINE}}/disk-config.nix


