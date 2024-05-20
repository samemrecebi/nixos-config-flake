{
  pkgs,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-graphical-gnome.nix"
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader = {
    grub.useOSProber = false;
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
  boot.supportedFilesystems = lib.mkForce ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];
  services.displayManager.autoLogin = lib.mkForce {
    enable = true;
    user = "emrecebi";
  };

  environment.systemPackages = with pkgs; [
    neofetch
    htop
    man
    sl
    wget
    curl
    pciutils
    libtool
    git
    stow
  ];
  nix.settings.experimental-features = ["nix-command" "flakes"];
}
