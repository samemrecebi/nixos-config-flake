{
  pkgs,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-graphical-calamares-plasma5.nix"
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader = {
    grub.useOSProber = false;
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = false;
  };

  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
  boot.supportedFilesystems = lib.mkForce ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];

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

    # Editor
    alejandra
    nil
  ];
  hardware.enableRedistributableFirmware = true;
  nix.settings.experimental-features = ["nix-command" "flakes"];
}
