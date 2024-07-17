{
  pkgs,
  outputs,
  ...
}: {
  imports = [
    outputs.homeManagerModules.common
    outputs.homeManagerModules.darwin
  ];

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
    ".config/zed/settings.json".source = ../../dotfiles/zed/settings.json;
  };

  # Emacs
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Darwin specific zsh configuration
  programs.zsh = {
    shellAliases = {
      updatesys = "darwin-rebuild switch --flake ~/.nix";
    };
    sessionVariables = {
      PYENV_ROOT = "$HOME/.pyenv";
      PATH = "$PYENV_ROOT/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/etc/profiles/per-user/emrecebi/bin/fzf:$PATH";
    };
    initExtra = ''
      eval "$(pyenv init --path)"
      eval "$(pyenv init -)"
    '';
  };

  # Enable Packages
  common-packages.enable = true;

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
