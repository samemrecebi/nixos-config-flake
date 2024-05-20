{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # No imports yet no modules
  imports = [
    outputs.darwinModules.python-global
  ];

  # Pkgs settings
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowAliases = false;
    };
  };

  # User defined
  users.users.emrecebi = {
    home = "/Users/emrecebi";
  };
nixpkgs.config.allowBroken = true;
  # Apps
  environment.systemPackages = with pkgs; [
    docker
    docker-compose
    coreutils
    moreutils
    mas
  ];

  # Mac homebrew (Nixpkgs gui apps wont work properly or missing)
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "zap";
    };
    taps = builtins.attrNames config.nix-homebrew.taps;
    brews = [
      # No brews rn
    ];
    casks = [
      "keka"
      "discord"
      "visual-studio-code"
      "spotify"
      "alacritty"
      "iina"
      "firefox"
      "tailscale"
      "todoist"
      "protonmail-bridge"
      "mac-mouse-fix"
      "whatsapp"
      "signal"
      "zulip"
      "termius"
      "telegram-desktop"
      "microsoft-word"
      "microsoft-powerpoint"
      "obsidian"
    ];
    masApps = {
      Bitwarden = 1352778147;
    };
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  programs.zsh.enable = true;

  # Nix relates settings
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    extra-platforms = x86_64-darwin aarch64-darwin
  '';
  system.stateVersion = 4;
  services.nix-daemon.enable = true;
  nix.settings.trusted-users = ["emrecebi"];
  security.pam.enableSudoTouchIdAuth = true;
}
