{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix

    # Common Components
    ../common/i18n.nix
    ../common/hyprland.nix
    ../common/nh.nix
    ../common/nix-ld.nix
    ../common/tailscale.nix
    ../common/stylix.nix
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
    ];
    blacklistedKernelModules = ["nouveau"];
    loader = {
      timeout = 0;
      systemd-boot.configurationLimit = 4;
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
  networking.hostName = "egger";
  services.resolved.enable = true;

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
    extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [ intel-vaapi-driver ];
    environment.sessionVariables = { LIBVA_DRIVER_NAME = "iHD"; };
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

  # Laptop power managment
  powerManagement.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 20;
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

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # System packages
  environment.systemPackages = with pkgs; [
    # Wayland Nvidia Hyprland Compatibility Apps
    egl-wayland
  ];

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
