{pkgs, ...}: {
  home.packages = with pkgs; [
    # LaTeX
    texliveMedium

    # Nix Related Packages
    alejandra
    nil
  ];
}
