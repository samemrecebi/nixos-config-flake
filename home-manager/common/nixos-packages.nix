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
    pkgs.awscli2
    pkgs.oci-cli
    ## Terraform
    pkgs.opentofu
    ## Database
    pkgs.tableplus

    # Productivity
    pkgs.todoist-electron

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
    pkgs.eog

    # Downloaders
    pkgs.yt-dlp
    pkgs.qbittorrent

    # Misc
    pkgs.hugo
    pkgs.protonmail-bridge-gui
    pkgs.mullvad-vpn
    pkgs.xdg-utils
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
