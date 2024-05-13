{
  description = "samemrecebi's NixOS Config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # My Nix Hardware Fork
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    # Formatter
    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
    # Nix Darwin
    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    # Nix Homebrew
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    # Homebrew taps
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    homebrew-bundle = {
      url = "github:homebrew/homebrew-bundle";
      flake = false;
    };
    homebrew-services = {
      url = "github:homebrew/homebrew-services";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nixos-hardware,
    alejandra,
    nix-darwin,
    nix-homebrew,
    homebrew-core,
    homebrew-cask,
    homebrew-bundle,
    homebrew-services,
    ...
  } @ inputs: let
    inherit (self) outputs;
    systems = [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in {
    # Custom packages
    packages = forAllSystems (system: import ./pkgs nixpkgs.legacyPackages.${system});
    formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.alejandra);
    overlays = import ./overlays {inherit inputs;};
    nixosModules = import ./modules/nixos;
    homeManagerModules = import ./modules/home-manager;
    darwinModules = import ./modules/darwin;

    # NixOS configuration entrypoint
    nixosConfigurations = {
      asus-a15 = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./nixos/a15/configuration.nix
          nixos-hardware.nixosModules.asus-fa507nv
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.emrecebi = import ./home-manager/a15/home.nix;
            home-manager.extraSpecialArgs = {inherit inputs outputs;};
          }
          {
            environment.systemPackages = [alejandra.defaultPackage.${system}];
          }
        ];
      };
      installerIso = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./nixos/iso/configuration.nix
        ];
      };
    };
    darwinConfigurations."Emres-MacBook-Pro" = nix-darwin.lib.darwinSystem rec {
      system = "aarch64-darwin";
      specialArgs = {inherit inputs outputs;};
      modules = [
        ./nixos/mbp/configuration.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.emrecebi = import ./home-manager/mbp/home.nix;
          home-manager.extraSpecialArgs = {inherit inputs outputs;};
        }
        nix-homebrew.darwinModules.nix-homebrew
        {
          nix-homebrew = {
            enable = true;
            enableRosetta = true;
            user = "emrecebi";
            taps = {
              "homebrew/homebrew-core" = homebrew-core;
              "homebrew/homebrew-cask" = homebrew-cask;
              "homebrew/homebrew-bundle" = homebrew-bundle;
              "homebrew/homebrew-services" = homebrew-services;
            };
            mutableTaps = false;
          };
        }
        {
          environment.systemPackages = [alejandra.defaultPackage.${system}];
        }
      ];
    };
  };
}
