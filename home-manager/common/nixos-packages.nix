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
