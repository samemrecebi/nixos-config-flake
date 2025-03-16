{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../../../home-manager/common/shell.nix
    ../common/common-packages.nix
  ];

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../../dotfiles/starship/starship.toml;
    ".emacs.d/init.el".source = ../../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../../dotfiles/emacs/early-init.el;
  };

  # Darwin specific zsh configuration
  programs.zsh = {
    shellAliases = {
      updatesys = "darwin-rebuild switch --flake ~/.nix";
    };
    sessionVariables = {
      PATH = "$JAVA_HOME/bin:/Users/emrecebi/.local/bin:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib:/opt/homebrew/bin:/opt/homebrew/sbin:/etc/profiles/per-user/emrecebi/bin/fzf:$PATH";
      LIBRARY_PATH = ":/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib:$LIBRARY_PATH";
      FLAKE = "/home/emrecebi/.nix-config";
      NVM_DIR = "/Users/emrecebi/.nvm";
    };
    initExtra = ''
      eval "$(pixi completion --shell zsh)"
      eval "$(uv generate-shell-completion zsh)"
      eval "$(uvx --generate-shell-completion zsh)"
      export NVM_DIR="$HOME/.nvm"
      [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
      [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
    '';
    profileExtra = ''
      eval "$(/opt/homebrew/bin/brew shellenv)"
      export NVM_DIR="$HOME/.nvm"
      [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
      [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
    '';
  };

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
