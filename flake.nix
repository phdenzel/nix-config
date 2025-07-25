{
  description = ''
    The NixOS configuration for all my machines
      #phinix:    workstation
      #sol:       AMD AI NUC
      #asahi:     macOS laptop
      #fenrix:    lenovo laptop
      #ygdrasil:  NAS server
      #durathror: kubernetes cluster node
      #dvalar:    kubernetes cluster node
      #duneyr:    kubernetes cluster node
      #heimdall:  raspberry pi
      #munin:     raspberry pi
      #hugin:     raspberry pi
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
    };

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    openconnect-sso = {
      url = "git+https://git@github.com/jcszymansk/openconnect-sso";
      inputs.nixpkgs.follows = "nixpkgs-openconnect-sso";
    };
     nixpkgs-openconnect-sso.url = "github:nixos/nixpkgs/46397778ef1f73414b03ed553a3368f0e7e33c2f";
    

    phd-wallpapers = {
      url = "git+ssh://git@github.com/phdenzel/wallpapers";
      flake = false;
    };
    phd-ark-modeline = {
      url = "git+https://git@github.com/phdenzel/phd-ark-modeline";
      flake = false;
    };
    phd-ark-tabline = {
      url = "git+https://git@github.com/phdenzel/phd-ark-tabline";
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
    openconnect-sso,  
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
    nixosMachine = {name, system ? "x86_64-linux"}: {
      "${name}" = lib.nixosSystem {
        specialArgs = {inherit self inputs outputs;};
        system = "${system}";
        modules = [
          (./. + "/hosts/${name}")
        ];
      };
    };
    nixosMachineWithHM = {name, system ? "x86_64-linux"}: {
      "${name}" = lib.nixosSystem {
        specialArgs = {inherit self inputs outputs;};
        system = "${system}";
        modules = [
          (./. + "/hosts/${name}")
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
    hmMachineConf = {name, system ? "x86_64-linux"}: {
      "phdenzel@${name}" = lib.homeManagerConfiguration {
        extraSpecialArgs = {inherit inputs outputs;};
        pkgs = pkgsFor."${system}";
        modules = [
          (./. + "/home/phdenzel/${name}.nix")
          ./home/phdenzel
        ];
      };
    };
  in {
    inherit lib;

    packages = (forEachSystem (pkgs: import ./pkgs {inherit pkgs;}));
    overlays = import ./overlays {inherit inputs outputs;};

    images = {
      iso = self.nixosConfigurations.iso.config.system.build.isoImage;
      rpi = self.nixosConfigurations.rpi.config.system.build.sdImage;
    };

    nixosConfigurations =
      (nixosMachineWithHM {name = "phinix";})
      // (nixosMachineWithHM {name = "sol";})
      // (nixosMachineWithHM {name = "fenrix";})
      // (nixosMachineWithHM {name = "ygdrasil";})
      // (nixosMachineWithHM {name = "idun";})
      // (nixosMachine {name = "heimdall"; system = "aarch64-linux";})
      // (nixosMachine {name = "rpi"; system = "aarch64-linux";})
      // (nixosMachine {name = "iso";});
      

    homeConfigurations =
      (hmMachineConf {name = "phinix";})
      // (hmMachineConf {name = "sol";})
      // (hmMachineConf {name = "fenrix";})
      // (hmMachineConf {name = "ygdrasil";})
      // (hmMachineConf {name = "idun";});
  };
}
