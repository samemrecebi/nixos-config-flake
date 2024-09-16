{pkgs, ...}: {
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  xdg.portal.enable = true;

  environment.sessionVariables.NIXOS_OZONE_WL = "1";
  environment.systemPackages = with pkgs; [gnomeExtensions.appindicator];
  services.udev.packages = with pkgs; [gnome-settings-daemon];

  environment.gnome.excludePackages = with pkgs; [
    gnome-tour
    gnome-connections
    gedit
  ];
}
