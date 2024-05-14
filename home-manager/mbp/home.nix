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

  home.file = {
    ".config/starship.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.config/starship.toml";
    ".config/alacritty/alacritty.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.config/alacritty/alacritty.toml";
    ".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.emacs.d/init.el";
    ".emacs.d/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.emacs.d/early-init.el";
  };

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
