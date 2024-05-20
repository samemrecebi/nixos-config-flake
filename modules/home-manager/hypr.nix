{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    outputs.homeManagerModules.theme
    outputs.homeManagerModules.waybar
  ];

  theme = {
    wallpaper = let
      url = "https://github.com/saint-13/Linux_Dynamic_Wallpapers/blob/main/Dynamic_Wallpapers/ChromeOSWind/ChromeOSWind-2.png?raw=true";
      sha256 = "0j4m3azrwgfh3rahmasv4c8pr40x1brbn77nx54hpmg8pb9i67cc";
      ext = "png";
    in
      builtins.fetchurl {
        name = "wallpaper-${sha256}.${ext}";
        inherit url sha256;
      };
  };

  home.pointerCursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 16;
    gtk.enable = true;
    x11.enable = true;
  };

  gtk = {
    enable = true;

    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };

    theme = {
      name = "adw-gtk3-dark";
      package = pkgs.adw-gtk3;
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    systemdIntegration = true;
    systemd.variables = ["--all"];
    settings = {
      "$mod" = "SUPER";
      "monitor" = ",highres,auto,1.25";

      env = [
        "NVD_BACKEND,direct"
        "WLR_DRM_DEVICES,$HOME/.config/hypr/cardFallback:$HOME/.config/hypr/card"
      ];

      exec = [
        "pkill waybar & sleep 0.5 && waybar"
      ];

      exec-once = [
        "hyprctl setcursor ${config.home.pointerCursor.name} ${toString config.home.pointerCursor.size}"
        "nm-applet --indicator"
        "dunst"
        "hyprpaper"
      ];

      input = {
        kb_layout = "us";
        follow_mouse = 1;
        touchpad.natural_scroll = false;
        sensitivity = 0; # -1.0 - 1.0, 0 means no modification.
      };

      xwayland.force_zero_scaling = true;

      general = {
        gaps_in = 5;
        gaps_out = 5;
        border_size = 1;
        "col.active_border" = "rgba(88888888)";
        "col.inactive_border" = "rgba(00000088)";

        allow_tearing = true;
        resize_on_border = true;
      };

      decoration = {
        rounding = 16;
        blur = {
          enabled = true;
          brightness = 1.0;
          contrast = 1.0;
          noise = 0.01;

          vibrancy = 0.2;
          vibrancy_darkness = 0.5;

          passes = 4;
          size = 7;

          popups = true;
          popups_ignorealpha = 0.2;
        };

        drop_shadow = true;
        shadow_ignore_window = true;
        shadow_offset = "0 2";
        shadow_range = 20;
        shadow_render_power = 3;
        "col.shadow" = "rgba(00000055)";
      };

      animations = {
        enabled = true;
        animation = [
          "border, 1, 2, default"
          "fade, 1, 4, default"
          "windows, 1, 3, default, popin 80%"
          "workspaces, 1, 2, default, slide"
        ];
      };

      misc = {
        # disable auto polling for config file changes
        disable_autoreload = true;

        force_default_wallpaper = 0;

        # disable dragging animation
        animate_mouse_windowdragging = false;

        # enable variable refresh rate (effective depending on hardware)
        vrr = 1;

        # we do, in fact, want direct scanout
        no_direct_scanout = false;
      };

      gestures = {
        workspace_swipe = true;
        workspace_swipe_forever = true;
      };

      binde = [
        ", XF86AudioRaiseVolume, exec, wpctl set-volume -l 1.4 @DEFAULT_AUDIO_SINK@ 5%+"
        ", XF86AudioLowerVolume, exec, wpctl set-volume -l 1.4 @DEFAULT_AUDIO_SINK@ 5%-"
        ", XF86MonBrightnessUp, exec, brillo -q -u 300000 -A 5"
        ", XF86MonBrightnessDown, exec, brillo -q -u 300000 -U 5"
      ];

      bind =
        [
          "$mod, F, exec, firefox"
          "$mod, G, fullscreen,"
          "$mod, RETURN, exec, alacritty"
          "$mod, Q, killactive,"
          "$mod, M, exit,"
          "$mod, F, exec, nautilus"
          "$mod, V, togglefloating,"
          "$mod, w, exec, wofi --show drun"

          # Move focus with mainMod + arrow keys
          "$mod, left, movefocus, l"
          "$mod, right, movefocus, r"
          "$mod, up, movefocus, u"
          "$mod, down, movefocus, d"
          ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ]
        ++ (
          # workspaces
          # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
          builtins.concatLists (builtins.genList (
              x: let
                ws = let
                  c = (x + 1) / 10;
                in
                  builtins.toString (x + 1 - (c * 10));
              in [
                "$mod, ${ws}, workspace, ${toString (x + 1)}"
                "$mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
              ]
            )
            10)
        );
    };
    extraConfig = ''
      exec-once = hyprctl hyprpaper preload "${config.theme.wallpaper}"
      exec-once = hyprctl hyprpaper wallpaper "${config.theme.wallpaper}"
    '';
  };

  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload = ${config.theme.wallpaper}
    wallpaper = , ${config.theme.wallpaper}
  '';
}
