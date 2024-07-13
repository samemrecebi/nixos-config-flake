{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    grub.enable =
      lib.mkEnableOption "Enable GRUB bootloader";
  };

  config = lib.mkIf config.grub.enable {
    boot = {
      kernelPackages = pkgs.linuxPackages_latest;
      consoleLogLevel = 0;
      kernelParams = [
        "quiet"
        "udev.log_level=0"
        "audit=0"
        "nowatchdog"
        "splash"
      ];
      plymouth = {
        enable = true;
        theme = "bgrt";
      };
      loader = {
        efi.canTouchEfiVariables = true;
        grub = {
          enable = true;
          efiSupport = true;
          configurationLimit = 10;
          devices = ["nodev"];
          useOSProber = true;
        };
      };
      initrd = {
        systemd.enable = true;
        verbose = false;
      };
    };
  };
}
