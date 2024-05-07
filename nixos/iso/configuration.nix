{
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-graphical-plasma5-new-kernel.nix"
  ];
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
