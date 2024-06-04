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
    outputs.darwinModules.dock
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

  system.activationScripts.postUserActivation.text = ''
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
  '';

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
      "neofetch"
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
      "ExifTool"
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

  system.defaults.dock = {
    orientation = "left";
    autohide = true;
    show-recents = false;
  };

  local = {
    dock.enable = true;
    dock.entries = [
      { path = "/Applications/Todoist.app/"; }
      { path = "/System/Applications/Calendar.app/"; }
      { path = "/Applications/Firefox.app/"; }
      { path = "/System/Applications/Mail.app/"; }
      { path = "/System/Applications/FaceTime.app/"; }
      { path = "/System/Applications/System Settings.app/"; }
      { path = "/Applications/WhatsApp.app/"; }
      { path = "/Applications/Signal.app/"; }
      { path = "/Applications/Discord.app/"; }
      { path = "/Applications/Alacritty.app"; }
      { path = "/Applications/Visual Studio Code.app/"; }
      { path = "/Applications/Emacs.app/"; }
      { path = "/Applications/Spotify.app/"; }
      {
        path = "${config.users.users.emrecebi.home}/Downloads";
        section = "others";
        options = "--sort name --view grid --display stack";
      }
    ];
  };

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
