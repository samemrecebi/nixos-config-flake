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
    outputs.homeManagerModules.common
    outputs.homeManagerModules.nixos
  ];

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
    ".config/zed/settings.json".source = ../../dotfiles/zed/settings.json;
  };

  # Nixpkgs overlays
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.stable-packages
    ];
  };

  # Packages
  ## Device spesific user packages
  home.packages = [
    pkgs.slack
    pkgs.teams-for-linux
    pkgs.zed-editor
    pkgs.nodejs
  ];

  ## Packages from modules
  common-packages.enable = true;
  nixos-packages.enable = true;
  firefox.enable = true;

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
