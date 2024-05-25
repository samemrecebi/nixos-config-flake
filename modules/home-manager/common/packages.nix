{
  config,
  pkgs,
  ...
}: let
  tex = pkgs.texlive.combine {
    inherit
      (pkgs.texlive)
      scheme-medium
      fontawesome5
      multirow
      moderncv
      beamer
      ;
  };
in {
  home.packages = with pkgs; [
    tex

    # Nix Related Packages
    alejandra
    nil
  ];
}
