{
  inputs,
  pkgs,
  ...
}: {
  # For xwayland
  services.xserver.enable = true;

  # Gnome stuff
  services = {
    gnome.gnome-keyring.enable = true;
    gvfs.enable = true;
  };

  # UWSM
  programs.uwsm.enable = true;
  programs.uwsm.waylandCompositors = {
    hyprland = {
      prettyName = "Hyprland";
      comment = "Hyprland compositor managed by UWSM";
      binPath = "/run/current-system/sw/bin/Hyprland";
    };
  };

  # Login
  services.greetd = {
    enable = true;
    vt = 3;
    settings = {
      default_session = {
        user = "greeter";
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time"; # start Hyprland with a TUI login manager
      };
    };
  };
  # unlock GPG keyring on login
  security.pam.services.greetd.enableGnomeKeyring = true;

  # Hyprland
  programs.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    portalPackage = pkgs.xdg-desktop-portal-hyprland;
    xwayland.enable = true;
    withUWSM = true;
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

  # Bluetooth applet
  services.blueman.enable = true;
}
