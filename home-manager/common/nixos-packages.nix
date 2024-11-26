{pkgs, ...}: {
  home.packages = [
    # Media
    pkgs.vlc

    # Browsers
    pkgs.librewolf-bin
    pkgs.google-chrome

    # Communication
    pkgs.thunderbird
    pkgs.element-desktop
    pkgs.whatsapp-for-linux
    pkgs.signal-desktop

    # Office Program
    pkgs.libreoffice-qt
    pkgs.hunspell
    pkgs.hunspellDicts.en_US
    pkgs.hunspellDicts.tr_TR

    # Document Viewer
    pkgs.zathura

    # Misc
    pkgs.protonmail-bridge-gui
    pkgs.mullvad-vpn

    # Moved from common-packages needs to be organized
    pkgs.pandoc
    pkgs.yt-dlp
    pkgs.htop
    pkgs.exiftool
    pkgs.hugo
    pkgs.wget
    pkgs.curl
    pkgs.alacritty
    pkgs.cmake
    pkgs.libtool
    pkgs.spotify
    pkgs.slack
    pkgs.zoom-us
    pkgs.discord
    pkgs.qbittorrent
  ];

  # Editors
  programs.vscode = {
    enable = true;
  };

  # Syncthing
  services.syncthing = {
    enable = true;
  };
}
