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
      pkgs.neofetch
      pkgs.sl
      pkgs.exiftool

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
      pkgs.whatsapp-for-linux

      # Developer Apps
      pkgs.alacritty
      pkgs.jetbrains.idea-ultimate
      pkgs.jetbrains.clion
      pkgs.jetbrains.webstorm

      # Office Program
      pkgs.libreoffice-qt
      pkgs.hunspell
      pkgs.hunspellDicts.en_US
      pkgs.hunspellDictTR.tr_TR
    ];
  };
}
