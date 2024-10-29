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
      # Browsers
      pkgs.librewolf-bin
      pkgs.google-chrome

      # Communication
      pkgs.thunderbird
      pkgs.element-desktop
      pkgs.whatsapp-for-linux

      # Office Program
      pkgs.libreoffice-qt
      pkgs.hunspell
      pkgs.hunspellDicts.en_US
      pkgs.hunspellDicts.tr_TR

      # Misc
      pkgs.protonmail-bridge-gui
      pkgs.mullvad-vpn
    ];
  };
}
