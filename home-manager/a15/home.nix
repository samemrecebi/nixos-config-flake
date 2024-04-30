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
    pkgs.alacritty
    pkgs.bitwarden-desktop
    pkgs.protonmail-bridge-gui
    pkgs.thunderbird
    pkgs.firefox
    pkgs.spotify
    pkgs.todoist-electron
    pkgs.termius
    pkgs.qbittorrent
    pkgs.mpv
    pkgs.zoom-us
    pkgs.yubikey-manager
    pkgs.ferdium
    pkgs.tailscale
    pkgs.texlive.combined.scheme-medium
    pkgs.imagemagick

    # Shell programs
    pkgs.zoxide
    pkgs.stow

    # Office Program
    pkgs.libreoffice-qt
    pkgs.hunspell
    pkgs.hunspellDicts.en_US

    # Dev
    pkgs.jetbrains.idea-ultimate

    # Fonts
    pkgs.b612
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

  # Editors
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # GPG
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # Shell and related programs
  programs.starship.enable = true;
  programs.git.enable = true;

  # Reload system units when config is changed
  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;
}
