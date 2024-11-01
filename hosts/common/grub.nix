{
  boot = {
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
}
