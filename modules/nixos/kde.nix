{pkgs, ...}: {
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Tripple buffering
  nixpkgs.overlays = [
    (final: prev: {
      gnome = prev.gnome.overrideScope (gnomeFinal: gnomePrev: {
        mutter = gnomePrev.mutter.overrideAttrs (old: {
          src = pkgs.fetchgit {
            url = "https://gitlab.gnome.org/vanvugt/mutter.git";
            rev = "663f19bc02c1b4e3d1a67b4ad72d644f9b9d6970";
            sha256 = "sha256-I1s4yz5JEWJY65g+dgprchwZuPGP9djgYXrMMxDQGrs=";
          };
        });
      });
    })
  ];

  environment.systemPackages = with pkgs; [gnomeExtensions.appindicator];
  services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];

  environment.gnome.excludePackages =
    (with pkgs; [
      # for packages that are pkgs.***
      gnome-tour
      gnome-connections
            gedit # text editor
    ])
    ++ (with pkgs.gnome; [
      # for packages that are pkgs.gnome.***
      epiphany # web browser
      geary # email reader
      cheese # webcam tool
      gnome-music
      gnome-terminal

      totem # video player
      tali # poker game
      iagno # go game
      hitori # sudoku game
      atomix # puzzle game
    ]);
}
