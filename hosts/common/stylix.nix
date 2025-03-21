{
  pkgs,
  inputs,
  ...
}: {
  # Setup stylix base values
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/monokai.yaml";
    polarity = "dark";
    image = ../../assets/linux-wallpaper.jpg;
    fonts = {
      monospace = {
        package = inputs.nonfree-fonts.packages.${pkgs.stdenv.hostPlatform.system}.berkeley-nf;
        name = "BerkeleyMono Nerd Font";
      };
      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };
      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };
      sizes = {
        applications = 14;
        desktop = 12;
        popups = 12;
        terminal = 14;
      };
    };
    targets = {
      grub.enable = false;
      grub.useImage = false;
    };
  };
}
