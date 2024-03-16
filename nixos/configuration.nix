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
    inputs.home-manager.nixosModules.default
  ];

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
    kernelParams = ["quiet" "udev.log_level=0"];
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
  };

  boot.initrd.luks.devices."luks-48e95629-d19a-4e8a-924e-53c660939c0c".device = "/dev/disk/by-uuid/48e95629-d19a-4e8a-924e-53c660939c0c";
  boot.initrd.systemd.enable = true;
  boot.initrd.verbose = false;

  networking.hostName = "asus-a15";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "nl_NL.UTF-8";
    LC_IDENTIFICATION = "nl_NL.UTF-8";
    LC_MEASUREMENT = "nl_NL.UTF-8";
    LC_MONETARY = "nl_NL.UTF-8";
    LC_NAME = "nl_NL.UTF-8";
    LC_NUMERIC = "nl_NL.UTF-8";
    LC_PAPER = "nl_NL.UTF-8";
    LC_TELEPHONE = "nl_NL.UTF-8";
    LC_TIME = "nl_NL.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable Gnome.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  environment.gnome.excludePackages =
    (with pkgs; [
      gnome-photos
      gnome-tour
    ])
    ++ (with pkgs.gnome; [
      cheese # webcam tool
      gnome-music
      epiphany # web browser
      geary # email reader
      gnome-characters
      tali # poker game
      iagno # go game
      hitori # sudoku game
      atomix # puzzle game
      yelp # Help view
      gnome-contacts
      gnome-initial-setup
    ]);
  programs.dconf.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Nvidia setup
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
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

  # Enable CUPS to print documents.
  services.printing.enable = true;

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

  # Trim
  services.fstrim.enable = lib.mkDefault true;

  # Shell
  users.defaultUserShell = pkgs.zsh;
  environment.shells = with pkgs; [zsh];
  programs.zsh.enable = true;

  # User
  users.users.emrecebi = {
    isNormalUser = true;
    description = "Emre Cebi";
    extraGroups = ["networkmanager" "wheel" "docker"];
  };

  # System packages
  environment.systemPackages = with pkgs; [
    gnome.gnome-tweaks
    yubikey-agent
    git
  ];
  virtualisation.docker.enable = true;

  # Extra system services
  services.tailscale.enable = true;
  services.tailscale.useRoutingFeatures = "client";
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];
  services.supergfxd.enable = true;
  services.asusd = {
    enable = true;
    enableUserService = true;
  };
  services.gnome.gnome-keyring.enable = true;
  systemd.user.services.protonmail-bridge = {
    description = "Protonmail Bridge";
    enable = true;
    script = "${pkgs.protonmail-bridge}/bin/protonmail-bridge --noninteractive --log-level info";
    wantedBy = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
  };

  nix.settings.experimental-features = ["nix-command" "flakes"];
  system.stateVersion = "23.11";
}
