{
  pkgs,
  inputs,
  ...
}: {
  home.packages = [
    pkgs.noto-fonts
    pkgs.nerd-fonts.noto
    pkgs.nerd-fonts.fira-code
    pkgs.nerd-fonts.hack
    pkgs.nerd-fonts.roboto-mono
    pkgs.nerd-fonts.jetbrains-mono
    inputs.nonfree-fonts.packages.${pkgs.stdenv.hostPlatform.system}.berkeley
    inputs.nonfree-fonts.packages.${pkgs.stdenv.hostPlatform.system}.berkeley-nf
    inputs.nonfree-fonts.packages.${pkgs.stdenv.hostPlatform.system}.comiccode
  ];
}
