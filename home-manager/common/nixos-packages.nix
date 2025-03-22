{pkgs, ...}: {
  home.packages = with pkgs; [
    # Development
    ## Generic
    gh
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
    # C/C++
    gcc
    # Terraform
    opentofu

    # Productivity
    todoist-electron

    # Media
    spotify

    # Communication
    protonmail-bridge-gui
    element-desktop
    wasistlos
    teams-for-linux
    discord
    slack
    signal-desktop

    # Password Managers
    bitwarden-desktop

    # Blogging
    hugo

    # Exporting
    pandoc

    # Downloaders
    yt-dlp
    qbittorrent

    # Latex
    texliveMedium
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

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30.override {withNativeCompilation = false;};
  };

  # better eval time
  manual.html.enable = false;
  manual.manpages.enable = false;
  manual.json.enable = false;
}
