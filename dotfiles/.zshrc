case "$OSTYPE" in
  darwin*)
    source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
    export TERM=alacritty

    autoload -Uz compinit && compinit
    zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

    alias em="emacsclient -c -n -a ''"
    alias -g updatesys="brew update && brew upgrade"

    eval "$(starship init zsh)"
    eval "$(zoxide init zsh)"
  ;;
  linux*)
    eval "$(starship init zsh)"

    HISTFILE=~/.zsh_history
    HISTSIZE=10000
    SAVEHIST=10000
    setopt appendhistory

    export TERM=alacritty

    autoload -Uz compinit && compinit
    zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

    alias em="emacsclient -c -n -a ''"
  ;;
esac