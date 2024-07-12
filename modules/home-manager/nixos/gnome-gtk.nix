{pkgs, ...}: {
  home.packages = with pkgs; [
    gnomeExtensions.tailscale-status
  ];

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      enable-hot-corners = true;
      edge-tiling = true;
    };
    settings."org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
    settings."org/gnome/mutter" = {
      experimental-features = ["scale-monitor-framebuffer"];
    };
    settings."org/gnome/desktop/background" = {
      picture-uri = "file:///home/emrecebi/.nix-config/static/space-wallpaper.jpg";
      picture-uri-dark = "file:///home/emrecebi/.nix-config/static/space-wallpaper.jpg";
    };
    settings."org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "tailscale-status@maxgallup.github.com"
        "apps-menu@gnome-shell-extensions.gcampax.github.com"
      ];
      favorite-apps = [
        "org.gnome.Nautilus.desktop"
        "todoist.desktop"
        "firefox.desktop"
        "thunderbird.desktop"
        "com.github.eneshecan.WhatsAppForLinux.desktop"
        "signal-desktop.desktop"
        "discord.desktop"
        "element-desktop.desktop"
        "Alacritty.desktop"
        "spotify.desktop"
        "code.desktop"
      ];
    };
  };
}
