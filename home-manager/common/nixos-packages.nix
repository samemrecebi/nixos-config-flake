{
  pkgs,
  lib,
  ...
}: {
  home.packages = with pkgs; [
    # Basic
    wget
    curl

    # Development
    ## Generic
    ghostty
    ## Python
    python3
    uv
    (pkgs.buildFHSEnv {
      name = "pixi";
      runScript = "pixi";
      targetPkgs = pkgs: with pkgs; [pixi];
    })
    ## JS/TS
    nodejs
    ## Terraform
    opentofu

    # Productivity
    todoist-electron

    # Media
    vlc
    spotify

    # Browsers
    firefox-bin
    google-chrome

    # Communication
    thunderbird
    protonmail-bridge-gui
    element-desktop
    wasistlos
    signal-desktop
    slack
    zoom-us
    discord
    teams-for-linux

    # Password Managers
    bitwarden-desktop

    # Office Program
    libreoffice-qt
    hunspell
    hunspellDicts.en_US
    hunspellDicts.tr_TR

    # Document Viewer
    zathura
    eog

    # Downloaders
    yt-dlp
    qbittorrent

    # VPN
    mullvad-vpn

    # Misc
    rpi-imager
  ];

  # VSCode
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  # Java
  programs.java.enable = true;

  # Common NixOS ZSH configuration
  programs.zsh = {
    sessionVariables = {
      FLAKE = "/home/emrecebi/.nix-config";
    };
  };

  # Syncthing
  services.syncthing = {
    enable = true;
  };
}
