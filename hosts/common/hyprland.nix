{
  inputs,
  pkgs,
  lib,
  config,
  ...
}: {
  # For xwayland
  services.xserver.enable = true;

  # Gnome stuff
  services = {
    # needed for GNOME services outside of GNOME Desktop
    dbus.packages = with pkgs; [
      gcr
      gnome.gnome-settings-daemon
    ];

    gnome.gnome-keyring.enable = true;

    gvfs.enable = true;
  };

  # Login
  services.greetd = {
    enable = true;
    vt = 3;
    settings = {
      default_session = {
        user = "emrecebi";
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd Hyprland"; # start Hyprland with a TUI login manager
      };
    };
  };
  # unlock GPG keyring on login
  security.pam.services.greetd.enableGnomeKeyring = true;

  # Hyprland
  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    portalPackage = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
    xwayland.enable = true;
  };

  # Electron app patch
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # XDG portal
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    config = {
      common.default = ["gtk"];
      hyprland.default = ["gtk" "hyprland"];
    };

    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };

  # Packages related to hyprland
  environment.systemPackages = with pkgs; [
    waybar
    dunst
    hyprpaper
    wofi
    networkmanagerapplet
    kdePackages.dolphin
  ];
}
