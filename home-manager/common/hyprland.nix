{
  wayland.windowManager.hyprland.enable = true;
  wayland.windowManager.hyprland.settings = {
    general = {
      gaps_in = 5;
      gaps_out = 5;
      border_size = 2;
      resize_on_border = false;
      allow_tearing = false;
      layout = "dwindle";
    };
    "$mod" = "SUPER";
    exec-once = [
      "nm-applet & dunst &"
      "waybar"
    ];
    gestures.workspace_swipe = true;
    bind =
      [
        "$mod, L, exec, librewolf"
        "$mod, Q, exec, alacritty"
        "$mod, R, exec, wofi --show drun"
        "$mod, M, exit"
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
  };
  wayland.windowManager.hyprland.extraConfig = ''
    monitor = , preferred, auto, 1
  '';
}
