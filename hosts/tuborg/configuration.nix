{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix

    # Common Components
    ../common/grub.nix
    ../common/i18n.nix
    ../common/hyprland.nix
    ../common/nh.nix
    ../common/nix-ld.nix
    ../common/tailscale.nix
    ../common/stylix.nix
    ../common/docker.nix
    ../common/printer.nix
    ../common/virt.nix
    ../common/fonts.nix
  ];

  # Nixpkgs config
  nixpkgs = {
    overlays = [
      (final: _prev: import ../../pkgs final.pkgs)
    ];
    config = {
      allowUnfree = true;
      allowAliases = false;
    };
  };

  # Bootloader.
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = with config.boot.kernelPackages; [
      cpupower
      nvidia_x11
    ];
    consoleLogLevel = 0;
    kernelParams = [
      "quiet"
      "loglevel=3"
      "audit=0"
      "nowatchdog"
      "splash"
      "boot.shell_on_fail"
      "udev.log_priority=3"
      "rd.udev.log_level=3"
      "rd.systemd.show_status=false"
      "pcie_aspm.policy=powersupersave"
      "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
      "amd_pstate.shared_mem=1"
      "amd_pstate=active"
    ];
    blacklistedKernelModules = ["nouveau"];
    initrd = {
      kernelModules = ["nvidia"];
      availableKernelModules = ["cryptd"];
      luks.devices."luks-48e95629-d19a-4e8a-924e-53c660939c0c".device = "/dev/disk/by-uuid/48e95629-d19a-4e8a-924e-53c660939c0c";
    };
    plymouth = {
      enable = true;
    };
  };

  # System Modules
  hardware.enableRedistributableFirmware = true;
  zramSwap.enable = true;

  # Devices firmware
  services.fwupd.enable = true;

  # Timezone
  services.automatic-timezoned.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;
  networking.hostName = "tuborg";
  services.resolved.enable = true;

  # Firewall
  networking.firewall.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us,tr";
  };

  # Graphics
  services.xserver.videoDrivers = ["nvidia"];
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  hardware.amdgpu.initrd.enable = true;
  # Nvidia extra settings (The actual setup is in nixos-hardware repo)
  hardware.nvidia = {
    open = true;
    modesetting.enable = true;
    package = config.boot.kernelPackages.nvidiaPackages.beta;
    dynamicBoost.enable = true;
  };

  # Asus related stuff
  services.supergfxd.enable = true;

  # Enable sound with pipewire.
  security.rtkit.enable = true;
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Laptop power managment
  powerManagement.enable = true;
  services.power-profiles-daemon.enable = false;
  services.auto-cpufreq.enable = true;
  services.auto-cpufreq.settings = {
    battery = {
      governor = "powersave";
      turbo = "never";
    };
    charger = {
      governor = "performance";
      turbo = "auto";
    };
  };

  # Bluetooth
  hardware.bluetooth.enable = true;

  # User
  users.users.emrecebi = {
    isNormalUser = true;
    description = "Emre Cebi";
    extraGroups = ["networkmanager" "wheel" "docker" "audio"];
  };

  # Shell
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # System packages
  environment.systemPackages = with pkgs; [
    # Wayland Nvidia Hyprland Compatibility Apps
    egl-wayland
  ];

  # Stylix
  stylix.targets = {
    grub.useImage = false;
  };

  # Emacs deamon
  services.emacs = {
    enable = true;
    package = pkgs.emacs30.override {withNativeCompilation = false;};
  };

  # Nix settings
  nix = {
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
    settings = {
      auto-optimise-store = true;
      experimental-features = "nix-command flakes";
      nix-path = config.nix.nixPath;
      substituters = ["https://hyprland.cachix.org"];
      trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
    };
    channel.enable = true;
  };
  system.stateVersion = "23.11";
}
