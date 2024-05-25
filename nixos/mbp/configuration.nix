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
  # Mac homebrew
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "zap";
    };
    global.brewfile = true;
    taps = [
      "homebrew/bundle"
      "homebrew/services"
    ];
    brews = [
      "docker"
      "docker-compose"
      "moreutils"
      "htop"
      "coreutils"
      "pandoc"
      "hugo"
      "yt-dlp"
      "sl"
      "neofetch"
      "wget"
      "pinentry-mac"
      "yubikey-personalization"
      "gnupg"
      "python"
      "pyenv"
      "node"
      "ykman"
      "llvm"
      "gcc"
    ];
    casks = [
      "gpg-suite"
      "temurin"
      "temurin@8"
      "jetbrains-toolbox"
      "docker"
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
    ];
    masApps = {
      Bitwarden = 1352778147;
      "Microsoft Word" = 462054704;
      "Microsoft PowerPoint" = 462062816;
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
