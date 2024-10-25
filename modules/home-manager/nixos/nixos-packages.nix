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
      # Yubikey
      pkgs.yubikey-agent

      # General packages
      pkgs.librewolf-bin
      pkgs.discord
      pkgs.element-desktop
      pkgs.bitwarden-desktop
      pkgs.protonmail-bridge-gui
      pkgs.thunderbird
      pkgs.signal-desktop
      pkgs.telegram-desktop
      pkgs.whatsapp-for-linux

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
  };
}
