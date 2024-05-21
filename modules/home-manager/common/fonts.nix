{pkgs, ...}: {
  home.packages = [
    pkgs.font-awesome
    (pkgs.nerdfonts.override {fonts = ["FiraCode" "Hack"];})
  ];
}
