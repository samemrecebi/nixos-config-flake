{ config, pkgs, ... }:

{
  home.username = "emrecebi";
  home.homeDirectory = "/home/emrecebi";
  home.stateVersion = "23.11"; # Please read the comment before changing.

  home.packages = [
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
    pkgs.cargo
    pkgs.rustc
    pkgs.nodejs
    pkgs.texlive.combined.scheme-medium
    pkgs.plantuml
    pkgs.graphviz
    (pkgs.nerdfonts.override { fonts = [ "FiraCode" "Hack" ]; })
  ];

  # Gnome Themeing
  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  };
  gtk.enable = true;
  qt.enable = true;
  qt.platformTheme = "gnome";
  qt.style.name = "adwaita-dark";
  qt.style.package = pkgs.adwaita-qt;

  # Shell
  programs.zsh.enable = true;
  programs.starship.enable = true;

  programs.git.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
  };

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ ];

  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

  programs.home-manager.enable = true;
}
