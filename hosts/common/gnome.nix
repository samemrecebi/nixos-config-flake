{
  inputs,
  pkgs,
  ...
}: {
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services = {
    gnome.gnome-keyring.enable = true;
    gvfs.enable = true;
  };
  environment.gnome.excludePackages = with pkgs; [
    orca
    evince
    geary
    gnome-disk-utility
    gnome-backgrounds
    gnome-tour # GNOME Shell detects the .desktop file on first log-in.
    gnome-user-docs
    baobab
    epiphany
    gnome-text-editor
    gnome-calculator
    gnome-calendar
    gnome-characters
    gnome-console
    gnome-logs
    gnome-maps
    gnome-music
    gnome-connections
    simple-scan
    snapshot
    totem
    yelp
    gnome-software
  ];
  environment.systemPackages = [pkgs.gnomeExtensions.appindicator];
  services.udev.packages = [pkgs.gnome-settings-daemon];
}
