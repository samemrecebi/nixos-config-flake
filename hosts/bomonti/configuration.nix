{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
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

  # Mac homebrew
  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    taps = [
      "homebrew/bundle"
      "homebrew/services"
    ];
    brews = [
      "htop"
      # Emacs Native Comp
      "libgccjit"
      "gmp"

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

      # Misc
      "syncthing"
    ];
    casks = [
      # Development
      "figma"

      # Communication
      "whatsapp"
      "signal"

      # Browsers
      {
        name = "librewolf";
        args = {no_quarantine = true;};
      }
      "arc"

      # Developer
      "docker"

      # System
      "mac-mouse-fix"

      # Games
      "zwift"

      # Office Suite
      "microsoft-excel"
      "microsoft-powerpoint"
      "microsoft-excel"

      # Misc
      "todoist"
      "tailscale"
      "protonmail-bridge"
      "mullvadvpn"
    ];
  };

  # Emacs deamon
  services.emacs = {
    enable = true;
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

  # Icons customization
  system = {
    activationScripts.postUserActivation.text = ''
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    '';
  };

  # Dock layout
  local.dock = {
    enable = true;
    entries = [
      {path = "/System/Applications/Calendar.app/";}
      {path = "/Applications/Arc.app/";}
      {path = "/Applications/Librewolf.app/";}
      {path = "/System/Applications/Mail.app/";}
      {path = "/Applications/WhatsApp.app/";}
      {path = "/Applications/Signal.app/";}
      {path = "/Users/emrecebi/Applications/Home Manager Apps/Alacritty.app";}
      {path = "/Users/emrecebi/Applications/Home Manager Apps/Visual Studio Code.app/";}
      {path = "/Users/emrecebi/Applications/Home Manager Apps/Spotify.app/";}
      {path = "/System/Applications/System Settings.app/";}
      {
        path = "${config.users.users.emrecebi.home}/Downloads";
        section = "others";
        options = "--sort name --view grid --display stack";
      }
    ];
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
