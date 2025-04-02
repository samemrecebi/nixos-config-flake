{pkgs, ...}: {
  home.packages = with pkgs; [
    # Development
    ## Python
    uv
    ## JS/TS
    nodejs
    # C/C++
    gcc

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

  # Common NixOS ZSH configuration
  programs.zsh = {
    sessionVariables = {
      PATH = "/home/emrecebi/.local/bin:$PATH";
      FLAKE = "/home/emrecebi/.nix-config";
    };
    initExtra = ''
      eval "$(uv generate-shell-completion zsh)"
      eval "$(uvx --generate-shell-completion zsh)"
    '';
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
