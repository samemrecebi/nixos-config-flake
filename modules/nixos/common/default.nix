{
  pkgs,
  lib,
  ...
}: {
  imports = [
    # Default NixOS modules
    ./auto-timezone.nix
    ./i18n.nix
    ./vm.nix
    ./nh.nix
    ./nix-ld.nix
    ./zsh.nix
    ./tailscale.nix

    # Optional NixOS modules
    ./grub.nix
    ./gaming.nix
  ];

  auto-timezone.enable = lib.mkDefault true;
  i18n.enable = lib.mkDefault true;
  vm.enable = lib.mkDefault true;
  nh.enable = lib.mkDefault true;
  nix-ld.enable = lib.mkDefault true;
  zsh.enable = lib.mkDefault true;
  tailscale.enable = lib.mkDefault true;
}
