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

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    phd-wallpapers = {
      url = "git+ssh://git@github.com/phdenzel/wallpapers";
      flake = false;
    };
    phd-ark-modeline = {
      url = "git+ssh://git@github.com/phdenzel/phd-ark-modeline";
      flake = false;
    };
    phd-ark-tabline = {
      url = "git+ssh://git@github.com/phdenzel/phd-ark-tabline";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    systems,
    disko,
    sops-nix,
    stylix,
    phd-wallpapers,
    phd-ark-modeline,
    ...
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
          home-manager.nixosModules.home-manager
          {
            home-manager.extraSpecialArgs = {inherit inputs;};
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.phdenzel = import ./home/phdenzel;
          }
        ];
      };

      idun = lib.nixosSystem {
        specialArgs = {inherit self inputs outputs;};
        modules = [
          ./hosts/idun
          home-manager.nixosModules.home-manager
          {
            home-manager.extraSpecialArgs = {inherit inputs;};
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.phdenzel = import ./home/phdenzel;
          }
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
          ./home/phdenzel
        ];
      };
      # VM
      "phdenzel@idun" = lib.homeManagerConfiguration {
        extraSpecialArgs = {inherit inputs outputs;};
        pkgs = pkgsFor.x86_64-linux;
        modules = [
          ./home/phdenzel/idun.nix
          ./home/phdenzel
        ];
      };
    };
  };
}
