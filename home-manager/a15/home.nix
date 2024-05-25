{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  home.username = "emrecebi";
  home.homeDirectory = "/home/emrecebi";
  home.stateVersion = "23.11"; # Please read the comment before changing.
  xdg.enable = true;

  imports = [
    outputs.homeManagerModules.common
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
    ];
  };

  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  };

  # Enable Packages
  common-packages.enable = true;
  nixos-packages.enable = true;

  # Per System packages
  home.packages = [
    # Empty for now
  ];

  programs.firefox = {
    enable = true;
  };

  # Editors
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
  };

  programs.zsh.sessionVariables = {
    FLAKE = "/home/emrecebi/.nix-config";
  };

  # GPG and SSH
  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    enableSshSupport = true;
    pinentryPackage = pkgs.pinentry-qt;
    enableScDaemon = true;
  };

  # Reload system units when config is changed
  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;
}
