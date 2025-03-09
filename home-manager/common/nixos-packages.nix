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
    ## PlantUML
    graphviz
    ## Editors / IDEs
    (pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.clion ["github-copilot"])
    (pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.pycharm-professional ["github-copilot"])
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
    ## Cloud access
    azure-cli
    awscli2
    oci-cli
    ## Terraform
    opentofu
    ## LaTeX LSP
    texlab

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
    protonmail-bridge-gui
    element-desktop
    wasistlos
    signal-desktop
    slack
    zoom-us
    discord
    teams-for-linux

    # Exporting
    pandoc

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
    hugo
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
