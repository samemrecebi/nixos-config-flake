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

  imports = [
    outputs.homeManagerModules.developer
    outputs.homeManagerModules.home-fonts
    outputs.homeManagerModules.home-shell
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.stable-packages
    ];
  };

  home.packages = [
    # General packages
    pkgs.firefox
    pkgs.discord
    pkgs.bitwarden-desktop
    pkgs.protonmail-bridge-gui
    pkgs.thunderbird
    pkgs.firefox
    pkgs.spotify
    pkgs.todoist-electron
    pkgs.qbittorrent
    pkgs.mpv
    pkgs.zoom-us
    pkgs.ferdium
    pkgs.tailscale
    pkgs.texlive.combined.scheme-medium
    pkgs.imagemagick

    # Office Program
    pkgs.libreoffice-qt
    pkgs.hunspell
    pkgs.hunspellDicts.en_US
  ];

  # GPG
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # SSH
  services.ssh-agent.enable = true;

  # Reload system units when config is changed
  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;
}
