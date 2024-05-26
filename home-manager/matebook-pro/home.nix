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
  # Enable Packages
  common-packages.enable = true;
  nixos-packages.enable = true;

  # Per System packages
  home.packages = [
    pkgs.gnomeExtensions.user-themes
    # Empty for now
  ];

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  home.sessionVariables.GTK_THEME = "Paper";

  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
    settings."org/gnome/mutter" = {
      experimental-features = ["scale-monitor-framebuffer"];
    };
    settings."org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "user-theme@gnome-shell-extensions.gcampax.github.com"
      ];
    };
  };

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
