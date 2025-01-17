{
  pkgs,
  inputs,
  ...
}: {
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

  # Fonts
  fonts.packages = with pkgs; [
    noto-fonts
    font-awesome
    liberation_ttf
    nerd-fonts.noto
    nerd-fonts.fira-code
    nerd-fonts.hack
    nerd-fonts.roboto-mono
    nerd-fonts.jetbrains-mono
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.berkeley
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.berkeley-nf
    inputs.nonfree-fonts.packages.${stdenv.hostPlatform.system}.comiccode
  ];

  # Disable stylix auto enable
  stylix.autoEnable = false;

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
      "hugo"
      "syncthing"
    ];
    casks = [
      # Development
      "alacritty"
      "ghostty"
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
      "balenaetcher"
      "raspberry-pi-imager"
      "tailscale"
      "proton-mail-bridge"
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
    NSGlobalDomain = {
      AppleICUForce24HourTime = true;
      AppleInterfaceStyle = "Dark";
      AppleMeasurementUnits = "Centimeters";
      AppleMetricUnits = 1;
    };
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
      AppleShowAllExtensions = true;
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
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
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
