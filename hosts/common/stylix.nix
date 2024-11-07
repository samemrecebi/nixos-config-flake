{pkgs, ...}: {
  # Setup stylix base values
  stylix = {
    enable = true;
    autoEnable = false;
    base16Scheme = {
      base00 = "111111";
      base01 = "222522";
      base02 = "303230";
      base03 = "808f80";
      base04 = "8fcfaf";
      base05 = "cfdfd5";
      base06 = "ffffff";
      base07 = "d0ffe0";
      base08 = "ef6560";
      base09 = "e09a0f";
      base0A = "d4aa02";
      base0B = "3fb83f";
      base0C = "6fc5ef";
      base0D = "37aff6";
      base0E = "d38faf";
      base0F = "ff9fbf";
    };
    image = ../../assets/linux-wallpaper.jpg;
    fonts = {
      monospace = {
        package = pkgs.nerdfonts.override {fonts = ["Hack"];};
        name = "Hack Nerd Font Mono";
      };
      serif = {
        package = pkgs.nerdfonts.override {fonts = ["Noto"];};
        name = "Noto Nerd Font";
      };
      sansSerif = {
        package = pkgs.nerdfonts.override {fonts = ["Noto"];};
        name = "Noto Nerd Font";
      };
      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };
    };
  };
}
