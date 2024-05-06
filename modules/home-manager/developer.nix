{pkgs, ...}: {
  home.packages = [
    pkgs.jetbrains.idea-ultimate
    pkgs.jetbrains.clion
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

  # Tools and programs
  programs.java = { enable = true; package = pkgs.oraclejre8; };
}
