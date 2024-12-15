{pkgs, inputs, ...}: {
  fonts.enableDefaultPackages = true;
  fonts.packages = with pkgs; [
    noto-fonts
    font-awesome
    liberation_ttf
    mplus-outline-fonts.githubRelease
    nerd-fonts.noto
    nerd-fonts.fira-code
    nerd-fonts.hack
    nerd-fonts.roboto-mono
    nerd-fonts.jetbrains-mono
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.berkeley
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.berkeley-nf
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.comiccode
  ];
  environment.sessionVariables = {
    FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
  };
}
