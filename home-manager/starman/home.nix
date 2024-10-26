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
    ../common/gnome-gtk.nix
  ];

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".config/alacritty/alacritty.toml".source = ../../dotfiles/alacritty/alacritty.toml;
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
  ];

  ## Packages from modules
  common-packages.enable = true;
  nixos-packages.enable = true;

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
