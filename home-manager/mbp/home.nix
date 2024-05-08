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

  home.file.".gitconfig".source = ../../dotfiles/.gitconfig;
  home.file.".config/starship.toml".source = ../../dotfiles/.config/starship.toml;
  home.file.".config/alacritty".source = ../../dotfiles/.config/alacritty;
  home.file.".emacs.d/init.el".source = ../../dotfiles/.emacs.d/init.el;
  home.file.".emacs.d/early-init.el".source = ../../dotfiles/.emacs.d/early-init.el;
}
 