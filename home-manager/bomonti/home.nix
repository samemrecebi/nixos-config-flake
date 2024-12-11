{pkgs, inputs, ...}:{
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

  home.packages = with pkgs; [
    inputs.nonfree-fonts.packages."aarch64-darwin".berkeley
    inputs.nonfree-fonts.packages."aarch64-darwin".comiccode
  ];

  # Darwin specific zsh configuration
  programs.zsh = {
    shellAliases = {
      updatesys = "darwin-rebuild switch --flake ~/.nix";
    };
    sessionVariables = {
      PYENV_ROOT = "$HOME/.pyenv";
      PATH = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib:$PYENV_ROOT/bin:/opt/homebrew/opt/make/libexec/gnubin:/opt/homebrew/bin:/opt/homebrew/sbin:/etc/profiles/per-user/emrecebi/bin/fzf:$PATH";
      LIBRARY_PATH = ":/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib:$LIBRARY_PATH";
      FLAKE = "/home/emrecebi/.nix-config";
    };
    initExtra = ''
      eval "$(pyenv init -)"
      eval "$(nodenv init - zsh)"
    '';
    profileExtra = ''
      eval "$(pyenv init -)"
    '';
  };

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
