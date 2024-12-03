{
  description = "samemrecebi's Nix configuration flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # Nix Hardware Fork
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    # My Nix Hardware Fork
    my-nixos-hardware.url = "github:samemrecebi/nixos-hardware/asus-tuf15-2023-simplify";
    # Hyprland
    hyprland.url = "github:hyprwm/Hyprland";
    # Stylix
    stylix.url = "github:danth/stylix";
    # Nix Darwin
    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nixos-hardware,
    my-nixos-hardware,
    hyprland,
    stylix,
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
    nixosModules = import ./modules/nixos;
    darwinModules = import ./modules/darwin;
    homeManagerModules = import ./modules/home-manager;

    # NixOS configuration entrypoint
    nixosConfigurations = {
      starman = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./hosts/starman/configuration.nix
          my-nixos-hardware.nixosModules.asus-fa507nv
          stylix.nixosModules.stylix
          home-manager.nixosModules.home-manager
          {
            # Home Manager as a module
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.extraSpecialArgs = {inherit inputs outputs;};
            home-manager.users.emrecebi = import ./home-manager/starman/home.nix;
            home-manager.sharedModules = [
              {
                stylix.targets = {
                  hyprland.enable = true;
                  waybar.enable = true;
                  wofi.enable = true;
                };
              }
            ];
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
        ./hosts/bomonti/configuration.nix
        stylix.darwinModules.stylix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "backup";
          home-manager.extraSpecialArgs = {inherit inputs outputs;};
          home-manager.users.emrecebi = import ./home-manager/bomonti/home.nix;
          home-manager.sharedModules = [
            {
              stylix.targets = {
                lazygit.enable = true;
              };
            }
          ];
        }
      ];
    };
  };
}
