{
  pkgs,
  config,
  ...
}: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    shellAliases = {
      em = "emacsclient -c -n -a ''";
      updatesys = "darwin-rebuild switch --flake ~/.nix-config";
    };
    history.size = 10000;
    history.path = "${config.xdg.dataHome}/zsh/history";
    sessionVariables = {
      DIRENV_LOG_FORMAT = ""; # silence direnv
      EDITOR = "code";
      NIXPKGS_ALLOW_UNFREE = "1";
    };
  };

  programs.starship.enable = true;
  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    extraConfig = {
      pull.rebase = false;
    };
    userName = "Emre Cebi";
    userEmail = "emre@cebi.io";
    extraConfig = {
      commit.gpgsign = true;
      gpg.format = "ssh";
      user.signingkey = "~/.ssh/id_sign.pub";
    };
  };

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
}
