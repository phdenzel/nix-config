{
  description = ''
    The NixOS configuration for all my machines
      #phinix:    workstation
      #fenrix:    laptop
      #idun:      VM for new config development
      #ygdrasil:  NAS server
      #durathror: kubernetes cluster node
      #dvalar:    kubernetes cluster node
      #dain:      kubernetes cluster node
      #munin:     raspberry pi
      #hunin:     raspberry pi
      #loki:      raspberry pi
  '';

  nixConfig = {
    experimental-features = [
      "flakes"
      "nix-command"
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # in case unstable nixpkgs won't build
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    hardware.url = "github:nixos/nixos-hardware";
    systems.url = "github:phdenzel/nix-systems/modern";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    systems,
    home-manager,
    disko,
    sops-nix,
  } @ inputs: let
    inherit (self) outputs;
    lib = nixpkgs.lib // home-manager.lib;
    forEachSystem = f: lib.genAttrs (import systems) (system: f pkgsFor.${system});
    pkgsFor = lib.genAttrs (import systems) (
      system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        }
    );
  in {
    inherit lib;

    packages = forEachSystem (pkgs: import ./pkgs {inherit pkgs;});
    overlays = import ./overlays {inherit inputs outputs;};

    nixosConfigurations = {
      phinix = lib.nixosSystem {
        specialArgs = {inherit self inputs outputs;};
        modules = [
          ./hosts/phinix
          inputs.disko.nixosModules.disko
        ];
      };
      idun = lib.nixosSystem {
        specialArgs = {inherit self inputs outputs;};
        modules = [
          ./hosts/idun
          inputs.disko.nixosModules.disko
        ];
      };
    };

    homeConfigurations = {
      # Main workstation
      "phdenzel@phinix" = lib.homeManagerConfiguration {
        extraSpecialArgs = {inherit inputs outputs;};
        pkgs = pkgsFor.x86_64-linux;
        modules = [
          ./home/phdenzel/phinix.nix
        ];
      };
      # VM
      "phdenzel@idun" = lib.homeManagerConfiguration {
        extraSpecialArgs = {inherit inputs outputs;};
        pkgs = pkgsFor.x86_64-linux;
        modules = [
          ./home/phdenzel/idun.nix
        ];
      };
    };
  };
}
