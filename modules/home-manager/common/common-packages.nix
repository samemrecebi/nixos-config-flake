{
  config,
  lib,
  pkgs,
  ...
}: let
  tex = pkgs.texlive.combine {
    inherit
      (pkgs.texlive)
      scheme-medium
      fontawesome5
      multirow
      moderncv
      beamer
      minted
      ;
  };
in {
  options = {
    common-packages.enable =
      lib.mkEnableOption "Enable Common Packages";
  };
  config = lib.mkIf config.common-packages.enable {
    home.packages = with pkgs; [
      # Media Packages
      spotify

      # Communication Packages
      slack
      zoom-us
      discord

      # Apps
      qbittorrent

      # Development Packages
      alacritty
      zed

      # Shell Packackes
      pandoc
      yt-dlp
      htop
      exiftool
      hugo
      wget
      curl
      yubikey-manager
      yubikey-personalization

      # Nix Related Packages
      alejandra
      nil
      nixd

      # Misc Packages
      tex
    ];

    programs.vscode = {
      enable = true;
    };
  };
}
