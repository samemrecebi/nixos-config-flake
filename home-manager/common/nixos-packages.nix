{
  pkgs,
  lib,
  ...
}: {
  home.packages = [
    # Basic
    pkgs.wget
    pkgs.curl

    # Development
    ## Generic
    pkgs.alacritty
    ## Cloud access
    pkgs.azure-cli
    pkgs.awscli2
    pkgs.oci-cli
    ## Terraform
    pkgs.opentofu

    # Productivity
    pkgs.todoist-electron

    # Media
    pkgs.vlc
    pkgs.spotify

    # Browsers
    pkgs.librewolf-bin
    pkgs.google-chrome

    # Communication
    pkgs.thunderbird
    pkgs.element-desktop
    pkgs.whatsapp-for-linux
    pkgs.signal-desktop
    pkgs.slack
    pkgs.zoom-us
    pkgs.webcord

    # Exporting
    pkgs.pandoc

    # Office Program
    pkgs.libreoffice-qt
    pkgs.hunspell
    pkgs.hunspellDicts.en_US
    pkgs.hunspellDicts.tr_TR

    # Document Viewer
    pkgs.zathura
    pkgs.eog

    # Downloaders
    pkgs.yt-dlp
    pkgs.qbittorrent

    # VPN
    pkgs.trayscale
    pkgs.mullvad-vpn

    # Misc
    pkgs.hugo
    pkgs.protonmail-bridge-gui
    pkgs.xdg-utils
  ];

  # Common NixOS ZSH configuration
  programs.zsh = {
    sessionVariables = {
      FLAKE = "/home/emrecebi/.nix-config";
    };
  };

  # Editors
  programs.vscode = {
    enable = true;
    package = pkgs.vscode.fhs;
  };

  programs.zed-editor = {
    enable = true;
    extraPackages = with pkgs; [
      typescript-language-server
      nixd
      alejandra
      clang
      clang-tools
    ];
    extensions = ["nix" "toml" "json" "html" "dockerfile" "make"];
    userSettings = {
      telemetry = {
        metrics = false;
        diagnostics = false;
      };
      confirm_quit = false;
      auto_update = false;

      base_keymap = "VSCode";
      theme = {
        mode = "dark";
        light = "Ayu Light";
        dark = "Ayu Dark";
      };

      ui_font_family = "BerkeleyMono Nerd Font";
      buffer_font_family = "BerkeleyMono Nerd Font";
      ui_font_size = 16;
      buffer_font_size = 15;

      inlay_hints = {
        enabled = true;
        show_type_hints = true;
        show_parameter_hints = true;
        show_other_hints = true;
      };

      load_direnv = "shell_hook";

      file_types = {
        Dockerfile = ["Dockerfile*"];
      };

      node = {
        path = lib.getExe pkgs.nodejs;
        npm_path = lib.getExe' pkgs.nodejs "npm";
      };

      lsp = {
        nixd = {
          binary = {
            path_lookup = true;
          };
          settings = {
            nixpkgs = {
              expr = "import <nixpkgs> { };";
            };
            formatting = {
              command = ["alejandra"];
            };
            options = {
              nixos = {
                expr = "(builtins.getFlake \"/home/emrecebi/.nix-config\").nixosConfigurations.egger.options";
              };
            };
          };
        };
        clangd = {
          binary = {
            path = lib.getExe pkgs.clang "clangd";
          };
        };
      };

      languages = {
        "Nix" = {
          language_servers = ["nixd" "!nil"];
          formatter = {
            external = {
              command = "alejandra";
            };
          };
        };
        "TypeScript" = {
          language_servers = ["typescript-language-server" "!vtsls" "..."];
        };
        "TSX" = {
          language_servers = ["typescript-language-server" "!vtsls" "..."];
        };
        "JavaScript" = {
          language_servers = ["typescript-language-server" "!vtsls" "..."];
        };
        "C" = {
          format_on_save = true;
          tab_size = 2;
        };
      };

      terminal = {
        env = {
          EDITOR = "zed --wait";
        };
        font_family = "BerkeleyMono Nerd Font";
        working_directory = "current_project_directory";
      };
    };
  };

  # Syncthing
  services.syncthing = {
    enable = true;
  };
}
