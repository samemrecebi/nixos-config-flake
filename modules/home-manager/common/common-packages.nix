{
  config,
  lib,
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
  options = {
    common-packages.enable =
      lib.mkEnableOption "Enable Common Packages";
  };
  config = lib.mkIf config.common-packages.enable {
    home.packages = with pkgs; [
      tex

      # Nix Related Packages
      alejandra
      nil
    ];
  };
}
