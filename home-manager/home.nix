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
    pkgs.font-awesome
    pkgs.liberation_ttf
    (pkgs.nerdfonts.override {fonts = ["FiraCode" "Hack"];})
  ];

  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  };

  gtk = {
    enable = true;

    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };

    theme = {
      name = "gruvbox-gtk-theme";
      package = pkgs.gruvbox-gtk-theme;
    };

    cursorTheme = {
      name = "Numix-Cursor";
      package = pkgs.numix-cursor-theme;
    };

    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };

    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  home.sessionVariables.GTK_THEME = "gruvbox-gtk-theme";

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
