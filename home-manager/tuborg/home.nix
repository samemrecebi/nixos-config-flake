{pkgs, ...}: {
  home.username = "emrecebi";
  home.homeDirectory = "/home/emrecebi";
  home.stateVersion = "23.11"; # Please read the comment before changing.
  xdg.enable = true;

  imports = [
    ../common/shell.nix
    ../common/common-packages.nix
    ../common/xdg.nix
    ../common/nixos-packages.nix
  ];

  # Dotfiles
  home.file = {
    ".config/starship.toml".source = ../../dotfiles/starship/starship.toml;
    ".emacs.d/init.el".source = ../../dotfiles/emacs/init.el;
    ".emacs.d/early-init.el".source = ../../dotfiles/emacs/early-init.el;
  };

  home.packages = with pkgs; [
    gnomeExtensions.tailscale-status
  ];

  # Gnome Home Settings
  dconf = {
    enable = true;
    settings = {
      "org/gnome/shell" = {
        disable-user-extensions = false;
        enabled-extensions = [
          "tailscale-status@maxgallup.github.com"
        ];
      };
      "org/gnome/desktop/interface" = {
        clock-show-weekday = true;
        color-scheme = "prefer-dark";
      };
    };
  };

  # GTK
  gtk = {
    enable = true;
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

  # GPG and SSH
  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    enableSshSupport = true;
    pinentryPackage = pkgs.pinentry-gnome3;
    enableScDaemon = true;
  };

  # Reload system units when config is changed
  systemd.user.startServices = "sd-switch";
  programs.home-manager.enable = true;
}
