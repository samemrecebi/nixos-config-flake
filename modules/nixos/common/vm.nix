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
    # KVM
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            })
            .fd
          ];
        };
      };
    };

    # Docker
    virtualisation.docker.enable = true;

    environment.systemPackages = with pkgs; [
      virt-manager
      qemu_kvm
      qemu
    ];

    users.users.emrecebi.extraGroups = ["docker" "libvirtd"];
  };
}
