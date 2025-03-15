{pkgs, ...}: {
  home.packages = with pkgs; [
    # Basic
    sl

    # Shared Development Packages
    alejandra
    nixd

    # Blogging
    hugo

    # Exporting
    pandoc

    # Downloaders
    yt-dlp

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
