{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    # Common Components
    ../common/common-setup.nix
  ];

  # Pkgs settings
  nixpkgs = {
    overlays = [
      (final: _prev: (import ../../../pkgs final.pkgs))
    ];
    config = {
      allowUnfree = true;
    };
  };

  # User defined
  users.users.emrecebi = {
    home = "/Users/emrecebi";
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
      experimental-features = nix-command flakes
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
    gc = {
      automatic = true;
      interval.Day = 7; #Hours, minutes
      options = "--delete-older-than 7d";
    };
  };

  ids.gids.nixbld = 350;
  system.stateVersion = 4;
  security.pam.services.sudo_local.touchIdAuth = true;
}
