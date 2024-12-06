{pkgs, ...}: {
  home.packages = with pkgs; [
    # Latex
    texliveMedium

    # Security Packages
    yubikey-manager
    yubikey-personalization
    yubikey-agent

    # LSP Packages
    texlab
    nil
    nixd

    # Nix Related Packages
    alejandra
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30.override {withNativeCompilation = false;};
  };
}
