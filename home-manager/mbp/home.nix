{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    outputs.homeManagerModules.common
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

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
  nixos-packages.enable = false;

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
