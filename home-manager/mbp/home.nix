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
  ];

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;

    shellAliases = {
      em = "emacsclient -c -n -a ''";
      updatesys = "darwin-rebuild switch --flake ~/.nix-config";
    };
    history.size = 10000;
    history.path = "${config.xdg.dataHome}/zsh/history";
    initExtra = ''
      . ~/.dotfiles/.zshrc
    '';
  };
  programs.starship.enable = true;
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  programs.git.enable = true;

  home.sessionVariables = {
    EDITOR = "em";
  };

  home.file = {
    ".gitconfig".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.gitconfig";
    ".config/starship.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.config/starship.toml";
    ".config/alacritty/alacritty.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.config/alacritty/alacritty.toml";
    ".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.emacs.d/init.el";
    ".emacs.d/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/.emacs.d/early-init.el";
  };

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
