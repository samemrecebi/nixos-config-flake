{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix

    # Common Components
    ../common/common-setup.nix
    ../common/grub.nix
    ../common/i18n.nix
    ../common/xfce.nix
    ../common/nix-ld.nix
    ../common/stylix.nix
    ../common/printer.nix
  ];

  # Nixpkgs config
  nixpkgs = {
    overlays = [
      (final: _prev: import ../../pkgs final.pkgs)
    ];
    config = {
      allowUnfree = true;
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
    ];
    blacklistedKernelModules = ["nouveau"];
    initrd = {
      kernelModules = ["nvidia"];
      availableKernelModules = ["cryptd"];
    };
  };

  # System Modules
  hardware.enableRedistributableFirmware = true;
  zramSwap.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;
  networking.hostName = "egger";

  # Firewall
  networking.firewall.enable = true;

  # Configure keymap in X11
  services.xserver.xkb.layout = "us,tr";

  # Graphics
  hardware.graphics.enable = true;

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
  services.auto-cpufreq = {
    settings = {
      battery = {
        governor = "powersave";
        turbo = "never";
      };
      charger = {
        governor = "performance";
        turbo = "auto";
      };
    };
  };

  # Bluetooth
  hardware.bluetooth.enable = true;

  # User
  users.users.emrecebi = {
    isNormalUser = true;
    description = "Emre Cebi";
    extraGroups = ["networkmanager" "wheel" "audio"];
  };

  # Shell
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # System packages
  environment.systemPackages = with pkgs; [
    # Empty for now
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
    };
  };
  system.stateVersion = "23.11";
}
