{
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-graphical-gnome.nix"
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  nixpkgs = {
    hostPlatform = "x86_64-linux";
    config = {
      allowUnfree = true;
      allowAliases = false;
    };
  };
  environment.systemPackages = with pkgs; [
    neofetch
    htop
    man
    sl
    wget
    curl
    pciutils
    libtool
    git
    stow
  ];
  nix.settings.experimental-features = ["nix-command" "flakes"];
}
