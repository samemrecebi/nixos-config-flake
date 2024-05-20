# Add your reusable home-manager modules to this directory, on their own file (https://nixos.wiki/wiki/Module).
# These should be stuff you would like to share with others, not your personal configurations.
{
  # List your module files here
  home-shell = import ./home-shell.nix;
  home-fonts = import ./home-fonts.nix;
  common = import ./common.nix;
  theme = import ./theme.nix;
  hypr = import ./hypr.nix;
  waybar = import ./waybar.nix;
}
