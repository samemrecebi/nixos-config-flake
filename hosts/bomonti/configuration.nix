{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    # Nothing here
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

  # Mac homebrew
  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    taps = [
      "homebrew/bundle"
      "homebrew/services"
    ];
    brews = [
      # Basic
      "yt-dlp"
      "htop"
      "exiftool"
      "wget"
      "curl"

      # PDF Tools dependencies
      "pkg-config"
      "poppler"
      "autoconf"
      "automake"

      # Development
      "python"
      "pyenv"
      "node"
      "opentofu"
      "azure-cli"
      "make"
      "gcc"
      "libgccjit"
      "hugo"

      # Misc
      "syncthing"
      "pandoc"
    ];
    casks = [
      # Development
      "alacritty"
      "figma"
      "visual-studio-code"
      "docker"
      "tableplus"

      # Virtualization
      "utm"

      # Communication
      "whatsapp"
      "signal"
      "slack"
      "zoom"
      "discord"

      # Media
      "spotify"
      "vlc"

      # Productivity
      "todoist"

      # Browsers
      {
        name = "librewolf";
        args = {no_quarantine = true;};
      }
      "arc"

      # System
      "mac-mouse-fix"

      # Games
      "zwift"

      # Office Suite
      "microsoft-excel"
      "microsoft-powerpoint"
      "microsoft-excel"

      # Misc
      "tailscale"
      "protonmail-bridge"
      "mullvadvpn"
    ];
  };

  # Emacs deamon
  services.emacs = {
    enable = false;
    package = pkgs.emacs;
  };

  programs.zsh.enable = true;

  # System Configuration
  system.defaults = {
    alf.globalstate = 1;
    dock = {
      orientation = "left";
      autohide = true;
      show-recents = false;
      wvous-bl-corner = 1;
      wvous-br-corner = 1;
      wvous-tl-corner = 1;
      wvous-tr-corner = 1;
    };
    finder = {
      FXPreferredViewStyle = "Nlsv";
      ShowPathbar = true;
    };
    CustomUserPreferences = {
      "com.apple.AdLib" = {
        allowApplePersonalizedAdvertising = false;
      };
      "com.apple.controlcenter" = {
        BatteryShowPercentage = true;
      };
      "com.apple.screencapture" = {
        location = "~/Desktop/Screenshots";
        type = "png";
      };
    };
  };

  # Applications fix
  system.activationScripts.applications.text = let
    env = pkgs.buildEnv {
      name = "system-applications";
      paths = config.environment.systemPackages;
      pathsToLink = "/Applications";
    };
  in
    pkgs.lib.mkForce ''
      # Set up applications.
      echo "setting up /Applications..." >&2
      rm -rf /Applications/Nix\ Apps
      mkdir -p /Applications/Nix\ Apps
      find ${env}/Applications -maxdepth 1 -type l -exec readlink '{}' + |
      while read -r src; do
        app_name=$(basename "$src")
        echo "copying $src" >&2
        ${pkgs.mkalias}/bin/mkalias "$src" "/Applications/Nix Apps/$app_name"
      done
    '';

  # Nix relates settings
  nix = {
    settings.trusted-users = ["emrecebi"];
    extraOptions = ''
      auto-optimise-store = true
      experimental-features = nix-command flakes
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
    gc = {
      automatic = true;
      interval.Day = 7; #Hours, minutes
      options = "--delete-older-than 7d";
    };
  };

  system.stateVersion = 4;
  services.nix-daemon.enable = true;
  security.pam.enableSudoTouchIdAuth = true;
}
