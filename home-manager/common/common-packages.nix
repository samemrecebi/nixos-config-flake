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
      cmake
      libtool

      # Shell Packackes
      tex
      pandoc
      yt-dlp
      htop
      exiftool
      hugo
      wget
      curl

      # Security Packages
      yubikey-manager
      yubikey-personalization
      yubikey-agent

      # LSP Packages
      texlab
      nil
      nixd

      # Nix Related Packages
      alejandra
    ];

    # Editors
    programs.vscode = {
      enable = true;
    };

    programs.emacs = {
      enable = true;
      package = pkgs.emacs;
    };
  };
}
