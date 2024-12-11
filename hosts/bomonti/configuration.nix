{pkgs, ...}: {
  imports = [
    # Common Components
    ../common/stylix.nix
  ];

  # Pkgs settings
  nixpkgs = {
    overlays = [
      (final: _prev: (import ../../pkgs final.pkgs))
    ];
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
      "wget"
      "curl"

      # PDF Tools dependencies
      "pkg-config"
      "poppler"
      "autoconf"
      "automake"

      # Development
      "coreutils"
      "binutils"
      # Python
      "python"
      "pyenv"
      # JS/TS
      "node"
      "nodenv"
      # Terraform
      "opentofu"
      # C/C++
      "make"
      "gcc"
      "bear"
      "ccls"
      "libgccjit"
      # Rust
      "rustup"
      # Cloud
      "azure-cli"
      "awscli"
      "oci-cli"

      # Exporting
      "pandoc"

      # Downloaders
      "yt-dlp"

      # Misc
      "hugo" # Static site generator
      "syncthing"
    ];
    casks = [
      # Development
      "alacritty"
      "figma"
      "docker"
      "tableplus"
      "macfuse"

      # Editors
      "visual-studio-code"
      "zed"
      "jetbrains-toolbox"

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

      # Downloaders
      "qbittorrent"

      # Misc
      "tailscale"
      "protonmail-bridge"
      "mullvadvpn"
    ];
  };

  # Emacs deamon
  services.emacs = {
    enable = true;
    package = pkgs.emacs30.override {withNativeCompilation = false;};
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
