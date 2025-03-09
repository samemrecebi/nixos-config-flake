{pkgs, ...}: {
  home.packages = with pkgs; [
    # Shared Development Packages
    alejandra
    nixd

    # Latex
    texliveMedium
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30.override {withNativeCompilation = false;};
  };

  # better eval time
  manual.html.enable = false;
  manual.manpages.enable = false;
  manual.json.enable = false;
}
