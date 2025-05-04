{
  description = ''
    The NixOS configuration for all my machines
      #phinix:    workstation
      #mani:      AMD AI NUC
      #asahi:     macOS laptop
      #fenrix:    lenovo laptop
      #ygdrasil:  NAS server
      #durathror: kubernetes cluster node
      #dvalar:    kubernetes cluster node
      #dain:      kubernetes cluster node
      #heimdall:  raspberry pi
      #munin:     raspberry pi
      #hunin:     raspberry pi
      #loki:      raspberry pi
      #idun:      VM for new config development
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

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
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
    nixosMachineWithHM = name: {
      "${name}" = lib.nixosSystem {
        specialArgs = {inherit self inputs outputs;};
        modules = [
          ./hosts/${name}
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
    hmMachineConf = name: {
      "phdenzel@${name}" = lib.homeManagerConfiguration {
        extraSpecialArgs = {inherit inputs outputs;};
        pkgs = pkgsFor.x86_64-linux;
        modules = [
          ./home/phdenzel/${name}.nix
          ./home/phdenzel
        ];
      };
    };
  in {
    inherit lib;

    packages = forEachSystem (pkgs: import ./pkgs {inherit pkgs;});
    overlays = import ./overlays {inherit inputs outputs;};

    nixosConfigurations =
      (nixosMachineWithHM "phinix")
      // (nixosMachineWithHM "fenrix")
      // (nixosMachineWithHM "idun");

    homeConfigurations =
      (hmMachineConf "phinix")
      // (hmMachineConf "fenrix")
      // (hmMachineConf "idun");
  };
}
