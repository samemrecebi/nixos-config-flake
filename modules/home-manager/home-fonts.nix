{pkgs, ...}: {
  home.packages = [
    pkgs.font-awesome
    pkgs.liberation_ttf
    pkgs.emacs-all-the-icons-fonts
    (pkgs.nerdfonts.override {fonts = ["FiraCode" "Hack"];})
  ];
}
