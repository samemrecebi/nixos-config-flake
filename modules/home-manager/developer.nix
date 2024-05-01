{pkgs, ...}: {
  home.packages = [
    pkgs.jetbrains.idea-ultimate
    pkgs.termius
  ];

  # Editors
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
}
