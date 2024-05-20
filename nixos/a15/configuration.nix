{
  inputs,
  outputs,
  options,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix

    # Modules
    outputs.nixosModules.nix-ld
    outputs.nixosModules.kvm
    outputs.nixosModules.zsh
    outputs.nixosModules.kde
    outputs.nixosModules.tailscale
    outputs.nixosModules.asusd
    outputs.nixosModules.i18n
    outputs.nixosModules.auto-timezone
    outputs.nixosModules.nh
    outputs.nixosModules.gaming
    outputs.nixosModules.bootloader
  ];

  # Nixpkgs config
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
    ];
    config = {
      allowUnfree = true;
      allowAliases = false;
    };
  };

  # Bootloader.
  boot = {
    extraModprobeConfig = "options kvm_amd nested=1";
    kernelParams = [
      "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
      "amdgpu"
      "smd_prefcore=enable"
      "iomem=relaxed"
      "pcie_aspm=force"
      "preempt=voluntary"
      "rtc_cmos.use_acpi_alarm=1"
    ];
    initrd = {
      luks.devices."luks-48e95629-d19a-4e8a-924e-53c660939c0c".device = "/dev/disk/by-uuid/48e95629-d19a-4e8a-924e-53c660939c0c";
    };
  };
  hardware.enableRedistributableFirmware = true;
  zramSwap.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;
  networking.hostName = "asus-a15";

  # Firewall
  networking.firewall.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Fonts TBD
  fonts.fontDir.enable = true;

  # Nvidia setup
  hardware.opengl = {
    enable = true;
    setLdLibraryPath = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vulkan-validation-layers
      intel-media-driver
      libvdpau-va-gl
    ];
  };
  hardware.nvidia = {
    powerManagement.enable = true;
    powerManagement.finegrained = true;
    package = config.boot.kernelPackages.nvidiaPackages.production;
  };
  # Nvidia-Docker
  virtualisation.docker.enableNvidia = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Laptop power managment
  powerManagement.enable = true;

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  # Mouse
  services.ratbagd.enable = true;

  # User
  users.users.emrecebi = {
    isNormalUser = true;
    description = "Emre Cebi";
    extraGroups = ["networkmanager" "wheel" "docker" "libvirtd"];
  };

  # Flatpak
  services.flatpak.enable = true;

  # System packages
  environment.systemPackages = with pkgs; [
  ];

  # Extra system services
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  nix = let
    flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
  in {
    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Opinionated: disable global registry
      flake-registry = "";
      # Workaround for https://github.com/NixOS/nix/issues/9574
      nix-path = config.nix.nixPath;
    };
    # Opinionated: disable channels
    channel.enable = false;

    # Opinionated: make flake registry and nix path match flake inputs
    registry = lib.mapAttrs (_: flake: {inherit flake;}) flakeInputs;
    nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}") flakeInputs;
  };
  system.stateVersion = "23.11";
}
