{pkgs, ...}: {
  services.emacs = {
    enable = true;
    package = pkgs.emacs.override {
      withNativeCompilation = true;
      withTreeSitter = true;
    };
  };

  # better eval time
  manual.html.enable = false;
  manual.manpages.enable = false;
  manual.json.enable = false;
}
