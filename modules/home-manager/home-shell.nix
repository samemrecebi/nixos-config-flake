{
  pkgs,
  config,
  lib,
  ...
}: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    shellAliases = {
      em = "emacsclient -c -n -a ''";
      ls = "ls --color";
      updatesys = "darwin-rebuild switch --flake ~/.nix";
    };
    sessionVariables = {
      DIRENV_LOG_FORMAT = ""; # silence direnv
      EDITOR = "code";
      NIXPKGS_ALLOW_UNFREE = "1";
    };
    plugins = [
      {
        name = "fzf-tab";
        src = pkgs.fetchFromGitHub {
          owner = "aloxaf";
          repo = "fzf-tab";
          rev = "v1.1.2";
          hash = "sha256-Qv8zAiMtrr67CbLRrFjGaPzFZcOiMVEFLg1Z+N6VMhg=";
        };
      }
    ];
    history.size = 10000;
    history.path = "${config.xdg.dataHome}/zsh/history";
    initExtra = ''
    zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
    zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'ls --color $realpath'
    '';
  };

  programs.starship.enable = true;
  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    userName = "Emre Cebi";
    userEmail = "emre@cebi.io";
    extraConfig = {
      commit.gpgsign = true;
      pull.rebase = false;
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
    options = ["--cmd cd"];
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
}
