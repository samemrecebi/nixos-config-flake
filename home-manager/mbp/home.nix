{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    outputs.homeManagerModules.home-fonts
    outputs.homeManagerModules.home-shell
    outputs.homeManagerModules.common-programs
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
  };

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
