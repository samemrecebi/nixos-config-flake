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
    outputs.nixosModules.common
    outputs.nixosModules.gnome
    outputs.nixosModules.asusd
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
    kernelModules = ["kvm-amd"];
    blacklistedKernelModules = [ "nouveau" ];
    kernelParams = [
      "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
      "smd_prefcore=enable"
      "mem_sleep_default=deep"
      "pcie_aspm.policy=powersupersave"
    ];
    initrd = {
      luks.devices."luks-48e95629-d19a-4e8a-924e-53c660939c0c".device = "/dev/disk/by-uuid/48e95629-d19a-4e8a-924e-53c660939c0c";
    };
  };
  grub.enable = true;

  # System Modules
  hardware.enableRedistributableFirmware = true;
  zramSwap.enable = true;

  # Firmware update
  services.fwupd.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;
  networking.hostName = "asus-a15";

  # Firewall
  networking.firewall.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us,tr";
  };

  # OpenGL
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  # Testing
  hardware.amdgpu.initrd.enable = true;

  # Nvidia extra settings (The actual setup is in nixos-hardware repo)
  hardware.nvidia = {
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

  # User
  users.users.emrecebi = {
    isNormalUser = true;
    description = "Emre Cebi";
    extraGroups = ["networkmanager" "wheel"];
  };

  # System packages
  environment.systemPackages = with pkgs; [
    # Empty for now
  ];

  # Extra system services
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Nix settings
  nix = let
    flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
  in {
    settings = {
      experimental-features = "nix-command flakes";
      flake-registry = "";
      nix-path = config.nix.nixPath;
    };
    channel.enable = false;
    registry = lib.mapAttrs (_: flake: {inherit flake;}) flakeInputs;
    nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}") flakeInputs;
  };
  system.stateVersion = "23.11";
}
