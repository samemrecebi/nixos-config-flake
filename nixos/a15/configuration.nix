{
  inputs,
  outputs,
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
    outputs.nixosModules.docker
    outputs.nixosModules.kde
    outputs.nixosModules.tailscale
    outputs.nixosModules.asusd
    outputs.nixosModules.i18n
    outputs.nixosModules.auto-timezone
    outputs.nixosModules.nh
  ];

  # Nixpkgs config
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.stable-packages
    ];
    config = {
      allowUnfree = true;
      allowAliases = false;
    };
  };

  # Bootloader.
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    consoleLogLevel = 0;
    kernelParams = ["quiet" "udev.log_level=0" "nvidia.NVreg_PreserveVideoMemoryAllocations=1"];
    plymouth = {
      enable = true;
      theme = "breeze";
    };
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        configurationLimit = 10;
        device = "nodev";
        useOSProber = true;
      };
    };
    initrd = {
      systemd.enable = true;
      systemd.network.wait-online.enable = false;
      verbose = false;
      luks.devices."luks-48e95629-d19a-4e8a-924e-53c660939c0c".device = "/dev/disk/by-uuid/48e95629-d19a-4e8a-924e-53c660939c0c";
    };
  };
  systemd.network.wait-online.enable = false;

  # Enable networking
  networking.networkmanager.enable = true;
  networking.hostName = "asus-a15";

  # Firewall
  networking.firewall.enable = true;

  #SSH
  services.openssh.enable = true;
  programs.ssh.startAgent = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  fonts.fontconfig.antialias = true;
  fonts.fontconfig.subpixel = {
    rgba = "none";
    lcdfilter = "none";
  };

  # Nvidia setup
  hardware.opengl = {
    enable = true;
    setLdLibraryPath = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vulkan-validation-layers
      intel-media-driver
      vaapiVdpau
      libvdpau-va-gl
    ];
  };
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.production;
    prime = {
      sync.enable = true;
      amdgpuBusId = "PCI:54:0:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

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
    neofetch
    htop
    man
    sl
    wget
    curl
    pciutils
    libtool
  ];

  # Extra system services
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  nix.settings.experimental-features = ["nix-command" "flakes"];
  system.stateVersion = "23.11";
}
