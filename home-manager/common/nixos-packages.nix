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
    ## Cloud access
    azure-cli
    awscli2
    oci-cli
    ## Terraform
    opentofu
    ## LaTeX LSP
    texlab
    ## Python Pixi
    (buildFHSUserEnv {
      name = "pixi";
      runScript = "pixi";
      targetPkgs = pkgs: with pkgs; [pixi];
    })
    ## Editors / IDEs
    code-cursor
    (pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.clion ["github-copilot"])
    (pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.pycharm-professional ["github-copilot"])

    # Productivity
    todoist-electron

    # Media
    vlc
    spotify

    # Browsers
    librewolf-bin
    google-chrome

    # Communication
    thunderbird
    element-desktop
    whatsapp-for-linux
    signal-desktop
    slack
    zoom-us
    webcord

    # Exporting
    pandoc

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
    trayscale
    mullvad-vpn

    # Misc
    hugo
    protonmail-bridge-gui
    xdg-utils
  ];

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
