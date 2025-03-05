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
      ls = "ls --color";
    };
    sessionVariables = {
      DIRENV_LOG_FORMAT = ""; # silence direnv
      EDITOR = "nano -w";
      VISUAL = "code --wait";
    };
    plugins = [
      {
        name = "fzf-tab";
        src = pkgs.fetchFromGitHub {
          owner = "aloxaf";
          repo = "fzf-tab";
          rev = "v1.2.0";
          hash = "sha256-q26XVS/LcyZPRqDNwKKA9exgBByE0muyuNb0Bbar2lY=";
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
    package = pkgs.git;
    userName = "Emre Cebi";
    userEmail = "emre@cebi.io";
    lfs.enable = true;
    extraConfig = {
      commit.gpgsign = true;
      pull.rebase = false;
      gpg.format = "ssh";
      user.signingkey = "~/.ssh/id_sign.pub";
      init.defaultBranch = "main";
    };
  };
  programs.lazygit.enable = true;
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
    tmux.enableShellIntegration = true;
  };
  programs.bat = {
    enable = true;
  };
  programs.ripgrep = {
    enable = true;
  };
}
