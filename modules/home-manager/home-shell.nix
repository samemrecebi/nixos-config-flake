{pkgs, ...}: {
  home.packages = [
    pkgs.alacritty
  ];

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
  programs.starship.enable = true;
  programs.git.enable = true;
}
