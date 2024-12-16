{
  pkgs,
  inputs,
  ...
}: {
  # Setup stylix base values
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-dark.yaml";
    polarity = "dark";
    image = ../../assets/linux-wallpaper.jpg;
    fonts = {
      monospace = {
        package = inputs.nonfree-fonts.packages.${pkgs.stdenv.hostPlatform.system}.berkeley-nf;
        name = "BerkeleyMono Nerd Font";
      };
      serif = {
        package = pkgs.nerd-fonts.noto;
        name = "NotoSerif Nerd Font";
      };
      sansSerif = {
        package = pkgs.nerd-fonts.noto;
        name = "NotoSans Nerd Font";
      };
      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };
      sizes = {
        applications = 12;
        desktop = 12;
        popups = 12;
        terminal = 12;
      };
    };
    targets = {
      grub.useImage = false;
    };
  };
}
