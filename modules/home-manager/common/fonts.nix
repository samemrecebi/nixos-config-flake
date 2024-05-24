{pkgs, ...}: {
  home.packages = [
    pkgs.font-awesome
    pkgs.noto-fonts
    (pkgs.nerdfonts.override {fonts = ["FiraCode" "Hack"];})
  ];
}
