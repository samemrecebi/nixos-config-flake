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
      "sl"

      # Development
      "coreutils"
      "binutils"
      # Python
      "python"
      "pixi"
      "uv"
      # JS/TS
      "node"
      "nodenv"
      # Terraform
      "opentofu"
      # C/C++
      "make"
      "gcc"
      "ccls"
      "clang-format"
      "libomp"
      # Go
      "golang"
      # Java
      "openjdk@21"
      # Cloud
      "azure-cli"
      "awscli"
      "oci-cli"

      # Exporting
      "pandoc"
      "graphviz"

      # Downloaders
      "yt-dlp"

      # Misc
      "hugo"
      "syncthing"
    ];
    casks = [
      # Development
      "ghostty"
      "docker"

      # Editors
      "visual-studio-code"
      "jetbrains-toolbox"

      # Communication
      "whatsapp"
      "signal"
      "slack"
      "zoom"
      "discord"
      "element"
      "microsoft-teams"

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
      "google-chrome"

      # System
      "mac-mouse-fix"

      # Games
      "zwift"

      # Office Suite
      "microsoft-word"
      "microsoft-powerpoint"

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
    settings = {
      trusted-users = ["emrecebi"];
      auto-optimise-store = true;
      experimental-features = "nix-command flakes";
    };
  };

  system.stateVersion = 4;
  security.pam.enableSudoTouchIdAuth = true;
}
