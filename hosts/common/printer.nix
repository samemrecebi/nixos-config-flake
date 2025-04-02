{pkgs, ...}: {
  # Printers
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      gutenprint
      brlaser
    ];
  };
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
}
