{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../common/shell.nix
    ../common/common-packages.nix
  ];

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
  };

  # Darwin specific zsh configuration
  programs.zsh = {
    shellAliases = {
      updatesys = "darwin-rebuild switch --flake ~/.nix";
    };
    sessionVariables = {
      PATH = "$JAVA_HOME/bin:/Users/emrecebi/.local/bin:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib:/opt/homebrew/opt/make/libexec/gnubin:/opt/homebrew/bin:/opt/homebrew/sbin:/etc/profiles/per-user/emrecebi/bin/fzf:$PATH";
      LIBRARY_PATH = ":/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib:$LIBRARY_PATH";
      FLAKE = "/home/emrecebi/.nix-config";
    };
    initExtra = ''
      eval "$(nodenv init - zsh)"
      eval "$(pixi completion --shell zsh)"
      eval "$(uv generate-shell-completion zsh)"
      eval "$(uvx --generate-shell-completion zsh)"
      export JAVA_HOME=$(/usr/libexec/java_home -v 23)
    '';
  };

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
