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

# Dock
  home.dock = {
    enable = true;
    entries = [
      { path = "/System/Library/CoreServices/Finder.app/"; }
      { path = "/Applications/Todoist.app/"; }
      { path = "/Applications/Firefox.app/"; }
      { path = "/Applications/Calendar.app/"; }
      { path = "/Applications/Mail.app/"; }
      { path = "/Applications/Facetime.app"; }
      { path = "/Applications/System Settings.app/"; }
      { path = "/Applications/WhatsApp.app/"; }
      { path = "/Applications/Signal.app/"; }
      { path = "/Applications/Discord.app/"; }
      { path = "/Applications/Alacritty.app/"; }
      { path = "/Applications/Visual Studio Code.app/"; }
      { path = "/Applications/Emacs.app/"; }
      { path = "/Applications/Spotify.app/"; }
      {
        path = "${config.home.homeDirectory}/Downloads/";
        section = "others";
        options = "--sort dateadded --view grid --display folder";
      }
    ];
  };

  # Enable Packages
  common-packages.enable = true;
  nixos-packages.enable = false;

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
