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
      minted
      ;
  };
in {
  options = {
    common-packages.enable =
      lib.mkEnableOption "Enable Common Packages";
  };
  config = lib.mkIf config.common-packages.enable {
    home.packages = with pkgs; [
      # Shell Packackes
      tex

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
  };
}
