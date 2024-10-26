{pkgs, ...}: {
  home.packages = [
    pkgs.noto-fonts
    (pkgs.nerdfonts.override {fonts = ["FiraCode" "Hack" "RobotoMono" "JetBrainsMono"];})
  ];
}
