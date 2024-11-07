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

    # Common Components
    ../common/grub.nix
    ../common/i18n.nix
    ../common/hyprland.nix
    ../common/nh.nix
    ../common/nix-ld.nix
    ../common/tailscale.nix
    ../common/stylix.nix
  ];

  # Nixpkgs config
  nixpkgs = {
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
    ];
    consoleLogLevel = 0;
    kernelParams = [
      "quiet"
      "log_level=3"
      "audit=0"
      "nowatchdog"
      "splash"
      "amd_pstate.shared_mem=1"
      "amd_pstate=passive"
    ];
    plymouth = {
      enable = true;
      theme = "bgrt";
    };
    blacklistedKernelModules = ["nouveau"];
    initrd = {
      luks.devices."luks-48e95629-d19a-4e8a-924e-53c660939c0c".device = "/dev/disk/by-uuid/48e95629-d19a-4e8a-924e-53c660939c0c";
    };
  };

  # System Modules
  hardware.enableRedistributableFirmware = true;
  zramSwap.enable = true;

  # Timezone
  services.automatic-timezoned.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;
  networking.hostName = "starman";

  # Firewall
  networking.firewall.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us,tr";
  };

  # OpenGL
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  # Initial amdgpu firmware
  hardware.amdgpu.initrd.enable = true;

  # Nvidia extra settings (The actual setup is in nixos-hardware repo)
  hardware.nvidia = {
    open = true;
    package = config.boot.kernelPackages.nvidiaPackages.beta;
  };

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

  # System Fonts
  fonts.enableDefaultPackages = true;
  fonts.packages = with pkgs; [
    liberation_ttf
    mplus-outline-fonts.githubRelease
  ];
  environment.sessionVariables = {
    FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
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

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # Microcode
  hardware.cpu.amd.updateMicrocode = true;

  # System packages
  environment.systemPackages = with pkgs; [
    # Wayland Nvidia Hyprland Compatibility Apps
    egl-wayland
  ];

  # Emacs deamon
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Nix settings
  nix = {
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
