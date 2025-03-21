{
  pkgs,
  inputs,
  ...
}: {
  # Common System Packages
  environment.systemPackages = with pkgs; [
    # Basic
    sl

    # Development
    ## Generic
    ghostty
    ## Nix
    alejandra
    nixd

    # Media
    vlc

    # Browsers
    firefox
    google-chrome

    # Communication
    thunderbird

    # Document Viewer
    zathura

    # Office Program
    libreoffice-qt
    hunspell
    hunspellDicts.en_US
    hunspellDicts.tr_TR
  ];

  # Common Fonts
  fonts.enableDefaultPackages = true;
  fonts.packages = with pkgs; [
    noto-fonts
    font-awesome
    liberation_ttf
    mplus-outline-fonts.githubRelease
    nerd-fonts.noto
    nerd-fonts.fira-code
    nerd-fonts.hack
    nerd-fonts.roboto-mono
    nerd-fonts.jetbrains-mono
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.berkeley
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.berkeley-nf
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.comiccode
  ];
  environment.sessionVariables = {
    FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
  };

  # Common nixos-rebuild cli
  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 4d --keep 3";
  };

  # Tailscale
  services.tailscale = {
    enable = true;
    package = pkgs.tailscale;
    useRoutingFeatures = "client";
    openFirewall = true;
  };
  networking.firewall.trustedInterfaces = ["tailscale0"];

  # Mullvad VPN
  services.resolved.enable = true; # Required for Mullvad VPN see https://discourse.nixos.org/t/connected-to-mullvadvpn-but-no-internet-connection/35803/8?u=lion
  services.mullvad-vpn = {
    enable = true;
    package = pkgs.mullvad-vpn;
  };
}
