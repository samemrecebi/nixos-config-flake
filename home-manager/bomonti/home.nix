{
  pkgs,
  outputs,
  inputs,
  ...
}: {
  imports = [
    ../common/shell.nix
    ../common/fonts.nix
    ../common/common-packages.nix
  ];

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".config/zed/settings.json".source = ../../dotfiles/zed/settings.json;
  };

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
    ];
  };

  # Enable Packages
  common-packages.enable = true;

  # Machine packages
  home.packages = with pkgs; [
    # Development
    docker
    utm
  ];

  # Darwin specific zsh configuration
  programs.zsh = {
    shellAliases = {
      updatesys = "darwin-rebuild switch --flake ~/.nix";
    };
    sessionVariables = {
      PATH = "/opt/homebrew/sbin:/etc/profiles/per-user/emrecebi/bin/fzf:$PATH";
    };
  };

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
