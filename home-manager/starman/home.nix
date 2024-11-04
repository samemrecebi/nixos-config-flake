{
  pkgs,
  outputs,
  ...
}: {
  home.username = "emrecebi";
  home.homeDirectory = "/home/emrecebi";
  home.stateVersion = "23.11"; # Please read the comment before changing.
  xdg.enable = true;

  imports = [
    ../common/shell.nix
    ../common/fonts.nix
    ../common/common-packages.nix
    ../common/xdg.nix
    ../common/nixos-packages.nix
    ../common/hyprland.nix
  ];

  programs.waybar.enable = true;

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
  };

  # Packages
  ## Device spesific user packages
  home.packages = [
  ];

  ## Packages from modules
  common-packages.enable = true;

  # Shell settings
  programs.zsh = {
    shellAliases = {
      # Empty for now
    };
    sessionVariables = {
      FLAKE = "/home/emrecebi/.nix-config";
    };
    initExtra = ''

    '';
  };

  # GPG and SSH
  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    enableSshSupport = true;
    pinentryPackage = pkgs.pinentry-gnome3;
    enableScDaemon = true;
  };

  # Reload system units when config is changed
  systemd.user.startServices = "sd-switch";
  programs.home-manager.enable = true;
}
