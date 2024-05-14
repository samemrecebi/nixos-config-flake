{pkgs, ...}: {
  programs.plasma = {
    enable = true;
    workspace = {
      clickItemTo = "select";
      lookAndFeel = "org.kde.breezedark.desktop";
      cursorTheme = "Bibata-Modern-Ice";
      iconTheme = "Papirus-Dark";
      wallpaper = "${pkgs.libsForQt5.plasma-workspace-wallpapers}/share/wallpapers/Patak/contents/images/1080x1920.png";
    };
  };
}
