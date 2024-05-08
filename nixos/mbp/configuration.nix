{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
  ];

  nix.package = pkgs.nix;
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowAliases = false;
    };
  };
  users.users.emrecebi = {
    home = "/Users/emrecebi";
  };

  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    extra-platforms = x86_64-darwin aarch64-darwin
  '';

  environment.systemPackages = with pkgs; [
    yt-dlp
    hugo
    imagemagick
    docker
    docker-compose
    gcc
    gnupg
    ispell
    pandoc
    pyenv
    yubikey-agent
  ];

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "zap";
    };
    casks = [
      "raycast"
      "keka"
      "discord"
      "visual-studio-code"
      "spotify"
      "alacritty"
      "iina"
      "steam"
      "firefox"
      "tailscale"
      "todoist"
      "protonmail-bridge"
      "mac-mouse-fix"
      "whatsapp"
      "signal"
      "zulip"
      "termius"
      "microsoft-word"
      "microsoft-powerpoint"
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
  services.nix-daemon.enable = true;
  security.pam.enableSudoTouchIdAuth = true;
}
