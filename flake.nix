{
  description = "samemrecebi's NixOS Config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # Nixpkgs stable
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";
    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # My Nix Hardware Fork
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    # Nix Darwin
    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-stable,
    home-manager,
    nixos-hardware,
    nix-darwin,
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

    # NixOS configuration entrypoint
    nixosConfigurations = {
      asus-a15 = nixpkgs.lib.nixosSystem {
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
        ];
      };
      installerIso = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./nixos/iso/configuration.nix
        ];
      };
    };
    darwinConfigurations = let
      inherit (inputs.nix-darwin.lib) darwinSystem;
    in {
        mbp = nixpkgs.lib.nixosSystem {
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
        ];
      };
    };
  };
}
