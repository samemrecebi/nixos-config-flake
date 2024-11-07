{
  wayland.windowManager.hyprland.enable = true;
  wayland.windowManager.hyprland.settings = {
    general = {
      gaps_in = 5;
      gaps_out = 5;
      border_size = 4;
      resize_on_border = false;
      allow_tearing = false;
      layout = "dwindle";
    };
    decoration = {
      rounding = 5;
      active_opacity = 1.0;
      inactive_opacity = 1.0;
      drop_shadow = false;
      shadow_range = 4;
      shadow_render_power = 3;
      blur = {
        enabled = false;
      };
    };
    misc.vfr = true;
    "$mod" = "SUPER";
    exec-once = [
      "udiskie"
      "wl-paste -t text --watch clipman store --no-persist"
      "systemctl --user start hyprpolkitagent"
      "nm-applet & dunst "
      "waybar"
    ];
    gestures.workspace_swipe = true;
    bind =
      [
        "$mod, L, exec, librewolf"
        "$mod, Q, exec, alacritty"
        "$mod, R, exec, wofi --show drun"
        "$mod, M, exit"
        "$mod, C, killactive,"

        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"

        "SUPER, V, exec, clipman pick -t wofi"
      ]
      ++ (
        # workspaces
        # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
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
    bindl = [
      ",XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
      ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
      ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
      ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
      ",XF86MonBrightnessUp, exec, brightnessctl s 10%+"
      ",XF86MonBrightnessDown, exec, brightnessctl s 10%-"
    ];
  };
  wayland.windowManager.hyprland.extraConfig = ''
    monitor = , preferred, auto, 1
  '';
}
