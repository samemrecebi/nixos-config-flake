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
      "log_level=3"
      "audit=0"
      "nowatchdog"
      "splash"
    ];
    plymouth = {
      enable = true;
      theme = "bgrt";
    };
    blacklistedKernelModules = ["nouveau"];
  };

  # System Modules
  hardware.enableRedistributableFirmware = true;
  zramSwap.enable = true;

  # Timezone
  services.automatic-timezoned.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;
  networking.hostName = "egger";

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
