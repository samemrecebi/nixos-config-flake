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
    outputs.nixosModules.kde
    outputs.nixosModules.tailscale
    outputs.nixosModules.lanzaboote
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

  # Boot
  systemd-boot.enable = true;

  boot.initrd.luks.devices."luks-282b478b-e75f-4ee5-a625-471a1e621e65".device = "/dev/disk/by-uuid/282b478b-e75f-4ee5-a625-471a1e621e65";

  # Flatpack
  services.flatpak.enable = true;

  hardware.enableRedistributableFirmware = true;
  zramSwap.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;
  networking.hostName = "matebook-pro";

  # Firewall
  networking.firewall.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us,tr";
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    setLdLibraryPath = true;
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

  # User
  users.users.emrecebi = {
    isNormalUser = true;
    description = "Emre Cebi";
    extraGroups = ["networkmanager" "wheel" "docker" "libvirtd"];
  };

  # System packages
  environment.systemPackages = with pkgs; [

  ];

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