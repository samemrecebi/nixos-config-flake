{pkgs, ...}: {
  home.packages = with pkgs; [
    # General Packages
    neofetch
    htop
    man
    sl
    wget
    curl
    pciutils
    libtool
    yt-dlp
    hugo
    imagemagick
    ispell
    gnupg
    pandoc
    yubikey-agent
    wget
    gnupg

    # LaTeX
    texliveMedium

    # Dev Packages
    gcc
    clang-tools
    nixpkgs-fmt
    nil
    nodejs
    nodePackages.npm
    rustfmt
    rust-analyzer
    nodePackages.prettier
    nodePackages.typescript-language-server
    gdb
  ];
}
