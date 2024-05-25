{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    systemd-boot.enable =
      lib.mkEnableOption "Enable Systemd-Boot bootloader";
  };

  config = lib.mkIf config.systemd-boot.enable {
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
        extraConfig = ''
          DeviceScale=1.50
        '';
      };
      loader = {
        efi.canTouchEfiVariables = true;
        systemd-boot.enable = true;
        systemd-boot.configurationLimit = 3;
      };
      initrd = {
        systemd.enable = true;
        verbose = false;
      };
    };
  };
}
