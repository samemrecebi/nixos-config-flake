{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        configurationLimit = 4;
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
