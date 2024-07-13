{
  pkgs,
  lib,
  ...
}: {
  imports = [
    # Default NixOS modules
    ./i18n.nix
    ./vm.nix
    ./nh.nix
    ./nix-ld.nix
    ./zsh.nix
    ./tailscale.nix
    ./grub.nix
  ];

  i18n.enable = lib.mkDefault true;
  vm.enable = lib.mkDefault true;
  nh.enable = lib.mkDefault true;
  nix-ld.enable = lib.mkDefault true;
  zsh.enable = lib.mkDefault true;
  tailscale.enable = lib.mkDefault true;
  grub.enable = lib.mkDefault true;
}
