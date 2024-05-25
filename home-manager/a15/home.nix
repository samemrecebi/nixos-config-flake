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
    outputs.homeManagerModules.qt
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

  # Enable Firefox
  firefox.enable = true;

  # Editors
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  programs.zsh.sessionVariables = {
    FLAKE = "/home/emrecebi/.nix-config";
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
