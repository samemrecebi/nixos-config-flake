{ pkgs, ... }:
 
{
  home.stateVersion = "23.11";
 
  home.packages = with pkgs; [

  ];

  programs.zsh = {
    enable = true;
    shellAliases = {
      em="emacsclient -c -n -a ''";
      updatesys="brew update && brew upgrade";
    };
  };
  programs.starship.enable = true;

  home.file.".gitconfig".source = ./dotfiles/.gitconfig;
}
 