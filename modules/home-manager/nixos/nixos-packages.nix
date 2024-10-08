{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    nixos-packages.enable =
      lib.mkEnableOption "Enable NixOS packages";
  };

  config = lib.mkIf config.nixos-packages.enable {
    home.packages = [
      # Terminal programs
      pkgs.yt-dlp
      pkgs.htop
      pkgs.sl
      pkgs.exiftool
      pkgs.hugo
      pkgs.wget
      pkgs.curl

      # Yubikey
      pkgs.yubikey-manager
      pkgs.yubikey-agent
      pkgs.yubikey-personalization

      # General packages
      pkgs.discord
      pkgs.element-desktop
      pkgs.bitwarden-desktop
      pkgs.protonmail-bridge-gui
      pkgs.thunderbird
      pkgs.spotify
      pkgs.todoist-electron
      pkgs.qbittorrent
      pkgs.mpv
      pkgs.zoom-us
      pkgs.signal-desktop
      pkgs.telegram-desktop
      pkgs.whatsapp-for-linux

      # Developer Apps
      pkgs.alacritty

      # Office Program
      pkgs.libreoffice-qt
      pkgs.hunspell
      pkgs.hunspellDicts.en_US
      pkgs.hunspellDicts.tr_TR
    ];

    ## VSCode
    programs.vscode = {
      enable = true;
      package = pkgs.vscode;
    };

    ## Emacs
    programs.emacs = {
      enable = true;
      package = pkgs.emacs;
    };
  };
}
