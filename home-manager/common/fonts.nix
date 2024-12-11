{pkgs,...}: {
  home.packages = with pkgs;[
    noto-fonts
    nerd-fonts.noto
    nerd-fonts.fira-code
    nerd-fonts.hack
    nerd-fonts.roboto-mono
    nerd-fonts.jetbrains-mono
  ];
}
