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
    pkgs.stow
    pkgs.firefox
    pkgs.discord
    pkgs.alacritty
    pkgs.bitwarden-desktop
    pkgs.protonmail-bridge
    pkgs.jetbrains.idea-ultimate
    pkgs.vscode
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
    pkgs.jdk11
    pkgs.texlive.combined.scheme-medium
    pkgs.plantuml
    pkgs.graphviz
    pkgs.imagemagick
    # Fonts
    pkgs.font-awesome
    pkgs.liberation_ttf
    pkgs.emacs-all-the-icons-fonts
    (pkgs.nerdfonts.override {fonts = ["FiraCode" "Hack"];})
  ];

  # GPG
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # Shell and related programs
  programs.starship.enable = true;
  programs.git.enable = true;

  # Editor
  programs.emacs = {
    enable = true;
    package = pkgs.emacs; # replace with pkgs.emacs-gtk, or a version provided by the community overlay if desired.
  };
  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

  programs.home-manager.enable = true;
}
