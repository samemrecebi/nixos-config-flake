{pkgs, ...}: {
  home.packages = [
    # Latex
    pkgs.texliveMedium

    # Security Packages
    pkgs.yubikey-manager
    pkgs.yubikey-personalization
    pkgs.yubikey-agent

    # LSP Packages
    pkgs.texlab
    pkgs.nixd

    # Nix Related Packages
    pkgs.alejandra
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30.override {withNativeCompilation = false;};
  };
}
