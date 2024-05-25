# Stolen from ryan4yin
{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    vm.enable =
      lib.mkEnableOption "Enable virtualization support and Docker";
  };
  config = lib.mkIf config.vm.enable {
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
  };
}
