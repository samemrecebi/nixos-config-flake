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

  # Nixpkgs overlays
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.stable-packages
    ];
  };

  # Packages
  ## Per user packages
  home.packages = [
    # Empty for now
  ];

  ## Packages from modules
  common-packages.enable = true;
  nixos-packages.enable = true;
  firefox.enable = true;

  # Programs
  ## VSCode
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  ## Emacs
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Nixos specific zsh configurations
  programs.zsh.sessionVariables = {
    FLAKE = "/home/emrecebi/.nix-config";
  };

  # dconf settings
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
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
