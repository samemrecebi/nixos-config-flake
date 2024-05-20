{pkgs, ...}: {
  home.packages = [
    pkgs.font-awesome
    pkgs.liberation_ttf
    pkgs.emacs-all-the-icons-fonts
    pkgs.cantarell-fonts
    pkgs.noto-fonts-cjk-sans
    (pkgs.nerdfonts.override {fonts = ["FiraCode" "Hack"];})
  ];
}
