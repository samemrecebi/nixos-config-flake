# Add your reusable NixOS modules to this directory, on their own file (https://nixos.wiki/wiki/Module).
# These should be stuff you would like to share with others, not your personal configurations.
{
  # List your module files here
  nix-ld = import ./nix-ld.nix;
  kvm = import ./kvm.nix;
  asusd = import ./asusd.nix;
  docker = import ./docker.nix;
  kde = import ./kde.nix;
  tailscale = import ./tailscale.nix;
  zsh = import ./zsh.nix;
  i18n = import ./i18n.nix;
  auto-timezone = import ./auto-timezone.nix;
  nh = import ./nh.nix;
}
