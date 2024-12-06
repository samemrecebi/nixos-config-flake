{pkgs, ...}: {
  home.packages = [
    # Basic
    pkgs.wget
    pkgs.curl

    # Development
    ## Generic
    pkgs.alacritty
    pkgs.zed-editor
    ## Cloud access
    pkgs.azure-cli

    # Media
    pkgs.vlc
    pkgs.spotify

    # Browsers
    pkgs.librewolf-bin
    pkgs.google-chrome

    # Communication
    pkgs.thunderbird
    pkgs.element-desktop
    pkgs.whatsapp-for-linux
    pkgs.signal-desktop
    pkgs.slack
    pkgs.zoom-us
    pkgs.discord

    # Exporting
    pkgs.pandoc

    # Office Program
    pkgs.libreoffice-qt
    pkgs.hunspell
    pkgs.hunspellDicts.en_US
    pkgs.hunspellDicts.tr_TR

    # Document Viewer
    pkgs.zathura

    # Downloaders
    pkgs.yt-dlp
    pkgs.qbittorrent

    # Misc
    pkgs.hugo
    pkgs.protonmail-bridge-gui
    pkgs.mullvad-vpn
  ];

  # Editors
  programs.vscode = {
    enable = true;
    package = pkgs.vscode.fhs;
  };

  # Syncthing
  services.syncthing = {
    enable = true;
  };
}
