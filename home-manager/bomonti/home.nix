{
  pkgs,
  outputs,
  inputs,
  ...
}: {
  imports = [
    ../common/shell.nix
    ../common/fonts.nix
    ../common/common-packages.nix
  ];

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
  };

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
    ];
  };

  # Enable Packages
  common-packages.enable = true;

  # Machine packages
  home.packages = with pkgs; [
    # Development
    utm
  ];

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
      eval "$(pyenv init -)"
    '';
    profileExtra = ''
      eval "$(pyenv init -)"
    '';
  };

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
