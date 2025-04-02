{
  description = "samemrecebi's Nix configuration flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # My Nix Hardware Fork
    my-nixos-hardware.url = "github:samemrecebi/nixos-hardware/master";
    # Nix Darwin
    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    # Non-free fonts
    nonfree-fonts.url = "github:samemrecebi/font-flake";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    my-nixos-hardware,
    nix-darwin,
    nonfree-fonts,
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
    packages = forAllSystems (system: import ./pkgs nixpkgs.legacyPackages.${system});
    formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.alejandra);
    nixosModules = import ./modules/nixos;
    darwinModules = import ./modules/darwin;
    homeManagerModules = import ./modules/home-manager;

    # NixOS configuration entrypoint
    nixosConfigurations = {
      tuborg = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./hosts/tuborg/configuration.nix
          my-nixos-hardware.nixosModules.asus-fa507nv
          home-manager.nixosModules.home-manager
          {
            # Home Manager as a module
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.extraSpecialArgs = {inherit inputs outputs;};
            home-manager.users.emrecebi = import ./home-manager/tuborg/home.nix;
          }
        ];
      };
      egger = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./hosts/egger/configuration.nix
          my-nixos-hardware.nixosModules.huawei-machc-wa
          home-manager.nixosModules.home-manager
          {
            # Home Manager as a module
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.extraSpecialArgs = {inherit inputs outputs;};
            home-manager.users.emrecebi = import ./home-manager/egger/home.nix;
          }
        ];
      };
      installerIso = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./hosts/iso/configuration.nix
        ];
      };
    };
    darwinConfigurations."Emres-MacBook-Pro" = nix-darwin.lib.darwinSystem {
      specialArgs = {inherit inputs outputs;};
      system = "aarch64-darwin";
      modules = [
        ./darwin/nix-darwin/bomonti/configuration.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "backup";
          home-manager.extraSpecialArgs = {inherit inputs outputs;};
          home-manager.users.emrecebi = import ./darwin/home-manager/bomonti/home.nix;
        }
      ];
    };
  };
}
