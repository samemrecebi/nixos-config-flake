{
  pkgs,
  inputs,
  ...
}: {
  home.file = {
    ".config/uwsm/env".source = ../../dotfiles/uwsm/env;
    ".config/uwsm/env-hyprland".source = ../../dotfiles/uwsm/env-hyprland;
  };

  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    systemd.enable = false;
    xwayland.enable = true;
    settings = {
      disable_hyprland_logo = true;
      disable_splash_rendering = true;
      general = {
        gaps_in = 5;
        gaps_out = 10;
        border_size = 2;
        resize_on_border = true;
        allow_tearing = true;
        layout = "dwindle";
      };
      decoration = {
        rounding = 10;
        active_opacity = 1.0;
        inactive_opacity = 0.8;
        blur = {
          enabled = false;
        };
        shadow = {
          enabled = false;
        };
      };
      misc.vfr = true;
      "$mod" = "SUPER";
      exec-once = [
        "uwsm app -- udiskie"
        "uwsm app -- wl-paste -t text --watch clipman store --no-persist"
        "uwsm app -- trayscale --hide-window"
        "systemctl --user enable --now hyprpolkitagent.service"
        "systemctl --user enable --now hyprpaper.service"
        "systemctl --user enable --now hypridle.service"
        "systemctl --user enable --now waybar.service"
        "uwsm app -- nm-applet"
        "uwsm app -- blueman-applet"
        "uwsm app -- dunst"
      ];
      gestures.workspace_swipe = true;
      bind =
        [
          # App Execution
          "$mod, B, exec, uwsm app -- io.gitlab.LibreWolf.desktop"
          "$mod, Return, exec, uwsm app -- Alacritty.desktop"
          "$mod, N, exec, uwsm app -- org.gnome.Nautilus.desktop"

          # Spotlight
          "$mod, space, exec, uwsm app -- wofi --show drun"

          # Clipboard
          "$mod, V, exec, uwsm app -- clipman pick -t wofi"

          # Lock
          "$mod SHIFT, L, exec, uwsm app -- hyprlock"

          # Screenshot
          "$mod, PRINT, exec, uwsm app -- hyprshot -m window"
          ",PRINT, exec, uwsm app -- hyprshot -m output"
          "$mod SHIFT, PRINT, exec, uwsm app -- hyprshot -m region"

          # Hyprland keybindings
          "$mod SHIFT, Q, exec, uwsm stop"
          "$mod, Q, killactive,"
          "$mod, F, togglefloating,"
          "$mod, S, togglesplit,"
          "$mod, H, movefocus, l"
          "$mod, L, movefocus, r"
          "$mod, K, movefocus, u"
          "$mod, J, movefocus, d"
        ]
        ++ (
          # Workspaces
          builtins.concatLists (builtins.genList (
              i: let
                ws = i + 1;
              in [
                "$mod, code:1${toString i}, workspace, ${toString ws}"
                "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
              ]
            )
            9)
        );
      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];
      bindl = [
        ",XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
        ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
        ",XF86MonBrightnessUp, exec, brightnessctl s 10%+"
        ",XF86MonBrightnessDown, exec, brightnessctl s 10%-"
      ];
    };
  };

  # Hyprland wallpaper
  services.hyprpaper.enable = true;

  # Hyprland lock screen
  programs.hyprlock = {
    enable = true;
    extraConfig = ''
      background {
        path=/home/emrecebi/.nix-config/assets/linux-wallpaper.jpg
      }

      input-field {
        monitor =
        size = 50%, 50%
        outline_thickness = 2

        fade_on_empty = true
        font_family = NotoSans Nerd Font
        placeholder_text = <i>Password</i>
        rounding = 15

        position = 0, -20
        halign = center
        valign = center

        check_color=rgb(ffb454)
        fail_color=rgb(f07178)
        font_color=rgb(e6e1cf)
        inner_color=rgb(0f1419)
        outer_color=rgb(3e4b59)
      }
    '';
  };

  # Task Bar
  programs.waybar.enable = true;

  home.packages = with pkgs; [
    wofi
    hyprshot
    dunst
    networkmanagerapplet
    hyprpolkitagent
    nautilus
    clipman
    udiskie
    brightnessctl
  ];
}
