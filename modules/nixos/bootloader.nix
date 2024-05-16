{pkgs, ...}: {
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    consoleLogLevel = 0;
    kernelParams = [
      "quiet"
      "udev.log_level=0"
      "audit=0"
      "nowatchdog"
    ];
    plymouth = {
      enable = true;
      theme = "breeze";
    };
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        configurationLimit = 10;
        device = "nodev";
        useOSProber = true;
      };
    };
    initrd = {
      systemd.enable = true;
      systemd.network.wait-online.enable = false;
      verbose = false;
    };
  };
  systemd.network.wait-online.enable = false;
}
