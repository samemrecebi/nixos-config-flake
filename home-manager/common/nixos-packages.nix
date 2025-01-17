{
  pkgs,
  lib,
  ...
}: {
  home.packages = with pkgs; [
    # Basic
    wget
    curl

    # Development
    ## Generic
    alacritty
    ghostty
    ## Cloud access
    azure-cli
    awscli2
    oci-cli
    ## Terraform
    opentofu
    ## LaTeX LSP
    texlab

    # Productivity
    todoist-electron

    # Media
    vlc
    spotify

    # Browsers
    librewolf-bin
    google-chrome

    # Communication
    thunderbird
    element-desktop
    whatsapp-for-linux
    signal-desktop
    slack
    zoom-us
    webcord

    # Exporting
    pandoc

    # Office Program
    libreoffice-qt
    hunspell
    hunspellDicts.en_US
    hunspellDicts.tr_TR

    # Document Viewer
    zathura
    eog

    # Downloaders
    yt-dlp
    qbittorrent

    # VPN
    trayscale
    mullvad-vpn

    # Misc
    hugo
    protonmail-bridge-gui
    xdg-utils
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
      pyright
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
        typescript-language-server = {
          binary = {
            path_lookup = true;
          };
        };
        pyright = {
          binary = {
            path_lookup = true;
          };
        };
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
            path_lookup = true;
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
          format_on_save = "on";
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
