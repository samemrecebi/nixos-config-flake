{
  pkgs,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-graphical-calamares-gnome.nix"
  ];

  nixpkgs = {
    hostPlatform = lib.mkDefault "x86_64-linux";
    config.allowUnfree = true;
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking = {
    hostName = "iso";
  };

  boot.loader = {
    grub.useOSProber = false;
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = false;
  };

  boot.supportedFilesystems = lib.mkForce ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];

  systemd = {
    services.sshd.wantedBy = pkgs.lib.mkForce ["multi-user.target"];
    targets = {
      sleep.enable = false;
      suspend.enable = false;
      hibernate.enable = false;
      hybrid-sleep.enable = false;
    };
  };

  environment.systemPackages = with pkgs; [
    man
    wget
    curl
    pciutils
    libtool
    git
  ];
  hardware.enableRedistributableFirmware = true;
  nix.settings.experimental-features = ["nix-command" "flakes"];
}
