{pkgs, ...}: {
  home.packages = with pkgs; [
    # Shared Development Packages
    alejandra
    nixd
    # ghostty Broken on darwin

    # Latex
    texliveMedium

    # Security Packages
    yubikey-manager
    yubikey-personalization
    yubikey-agent
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30.override {withNativeCompilation = false;};
  };
}
