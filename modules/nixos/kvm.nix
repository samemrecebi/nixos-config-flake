# Stolen from ryan4yin
{pkgs, ...}: {
  boot.kernelModules = ["vfio-pci"];

  virtualisation = {
    docker = {
      enable = true;
      daemon.settings = {
        "features" = {"containerd-snapshotter" = true;};
      };
      enableOnBoot = true;
    };

    libvirtd = {
      enable = true;
      qemu.runAsRoot = true;
    };
    waydroid.enable = true;
    lxd.enable = true;
  };

  environment.systemPackages = with pkgs; [
    virt-manager
    qemu_kvm
    qemu
  ];
}
