{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  home.username = "emrecebi";
  home.homeDirectory = "/home/emrecebi";
  home.stateVersion = "23.11"; # Please read the comment before changing.
  xdg.enable = true;

  home.packages = [
    # General packages
    pkgs.stow
    pkgs.firefox
    pkgs.discord
    pkgs.alacritty
    pkgs.bitwarden-desktop
    pkgs.protonmail-bridge-gui
    pkgs.thunderbird
    pkgs.firefox
    pkgs.spotify
    pkgs.todoist-electron
    pkgs.termius
    pkgs.qbittorrent
    pkgs.mullvad-vpn
    pkgs.mpv
    pkgs.zoom-us
    pkgs.yubikey-manager
    pkgs.whatsapp-for-linux
    pkgs.signal-desktop
    pkgs.tailscale
    pkgs.texlive.combined.scheme-medium
    pkgs.imagemagick

    # Dev
    pkgs.gradle_6
    pkgs.jetbrains.idea-ultimate
    pkgs.plantuml
    pkgs.graphviz

    # Fonts
    pkgs.font-awesome
    pkgs.liberation_ttf
    pkgs.emacs-all-the-icons-fonts
    (pkgs.nerdfonts.override {fonts = ["FiraCode" "Hack"];})
  ];

  # Direnv
  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true; # see note on other shells below
      nix-direnv.enable = true;
    };
  };

  # Java
  programs.java = {
    enable = true;
    package = (pkgs.jdk11.override {enableJavaFX = true;});
  };

  # Editor
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  # GPG
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # Shell and related programs
  programs.starship.enable = true;
  programs.git.enable = true;

  programs.home-manager.enable = true;
}
