{pkgs, inputs, ...}: {
  # Setup stylix base values
  stylix = {
    enable = true;
    autoEnable = false;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-dark.yaml";
    image = ../../assets/linux-wallpaper.jpg;
    fonts = {
      monospace = {
        package = if pkgs.stdenv.isLinux then inputs.nonfree-fonts.packages."x86_64-linux".berkeley-nf else inputs.nonfree-fonts.packages."aarch64-darwin".berkeley-nf;
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
    };
  };
}
