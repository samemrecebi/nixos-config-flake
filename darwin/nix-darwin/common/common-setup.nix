{
  pkgs,
  inputs,
  ...
}: {
  # Nix System Packages
  environment.systemPackages = with pkgs; [
    # Development Packages
    ## Nix
    alejandra
    nixd
    ## Python
    pixi
    uv
    ## TS
    typescript-language-server

    # Blogging
    hugo

    # Exporting
    pandoc

    # Downloaders
    yt-dlp

    # Latex
    texliveMedium
  ];

  # Homebrew System Packages
  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    taps = [
      "homebrew/bundle"
      "homebrew/services"
    ];
    brews = [
      # Development
      ## Generic
      "cmake"
      ## C/C++
      "libomp"
      "llvm"
      "gcc"
      "libgccjit"
      ## Python
      "python"
      ## Pinecil
      "blisp"
    ];
    casks = [
      # Development
      ## Generic
      "ghostty"
      ## Editors
      "visual-studio-code"
      "zed"
      ## Containers
      "docker"

      # Browsers
      "firefox"
      "google-chrome"

      # Media
      "spotify"
      "vlc"

      # Communication
      "whatsapp"
      "signal"
      "slack"
      "discord"
      "microsoft-teams"
      "thunderbird"

      # Productivity
      "todoist"

      # System
      "mac-mouse-fix"

      # Downloaders
      "qbittorrent"

      # Office Suite
      "microsoft-word"
      "microsoft-powerpoint"
      "microsoft-excel"

      # VPN
      "mullvadvpn"
      "tailscale"

      # Misc
      "proton-mail-bridge"
    ];
  };

  # Fonts
  fonts.packages = with pkgs; [
    noto-fonts
    font-awesome
    nerd-fonts.noto
    nerd-fonts.fira-code
    nerd-fonts.hack
    nerd-fonts.roboto-mono
    nerd-fonts.jetbrains-mono
    nerd-fonts.symbols-only
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.berkeley
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.berkeley-nf
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.comiccode
  ];

  # Emacs deamon
  services.emacs = {
    enable = true;
    package = pkgs.emacs30.override {withNativeCompilation = false;};
  };
}
