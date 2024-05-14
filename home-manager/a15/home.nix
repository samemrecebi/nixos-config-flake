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
    outputs.homeManagerModules.home-fonts
    outputs.homeManagerModules.home-shell
    outputs.homeManagerModules.plasma
    outputs.homeManagerModules.common-programs
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
    ];
  };

  home.packages = [
    # General packages
    pkgs.firefox
    pkgs.discord
    pkgs.bitwarden-desktop
    pkgs.protonmail-bridge-gui
    pkgs.thunderbird
    pkgs.spotify
    pkgs.todoist-electron
    pkgs.qbittorrent
    pkgs.mpv
    pkgs.zoom-us
    pkgs.tailscale
    pkgs.signal-desktop
    pkgs.whatsapp-for-linux

    # Developer Apps
    pkgs.alacritty
    pkgs.jetbrains.idea-ultimate
    pkgs.jetbrains.clion
    pkgs.termius

    # Office Program
    pkgs.libreoffice-qt
    pkgs.hunspell
    pkgs.hunspellDicts.en_US
    pkgs.hunspellDictTR.tr_TR
  ];

  # Editors
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Tools and programs
  programs.java.enable = true;

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
  };

  programs.zsh.sessionVariables = {
    FLAKE = "/home/emrecebi/my-nixos-config";
  };

  # GPG
  programs.gpg.enable = true;
  services.gpg-agent.enable = false;

  # SSH
  services.ssh-agent.enable = true;

  # Reload system units when config is changed
  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;
}
