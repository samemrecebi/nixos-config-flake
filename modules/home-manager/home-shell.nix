{pkgs, ...}:
{
    home.packages = [
        pkgs.alacritty
        pkgs.stow 
        pkgs.zoxide
    ];

    programs = {
        direnv = {
            enable = true;
            enableZshIntegration = true; # see note on other shells below
            nix-direnv.enable = true;
        };
    };
    programs.starship.enable = true;
    programs.git.enable = true;
}