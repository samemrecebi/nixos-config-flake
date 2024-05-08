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

  # Dotfiles
  home.file = {
     ".zshrc".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.zshrc";   
    ".gitconfig".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.gitconfig";
    ".config/starship.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.config/starship.toml";
    ".config/alacritty/alacritty.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.config/alacritty/alacritty.toml";
    ".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.emacs.d/init.el";
    ".emacs.d/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.emacs.d/early-init.el";
  };

  # GPG
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # SSH
  services.ssh-agent.enable = true;

  # Reload system units when config is changed
  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;
}
