{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    nix-ld.enable =
      lib.mkEnableOption "Enable nix-ld";
  };
  config = lib.mkIf config.nix-ld.enable {
    # Enable nix ld
    programs.nix-ld.enable = true;

    programs.nix-ld.libraries = with pkgs; [
      stdenv.cc.cc
      zlib
      fuse3
      icu
      zlib
      nss
      openssl
      curl
      expat
      wget
      alsa-lib
      at-spi2-atk
      at-spi2-core
      atk
      cairo
      cups
      curl
      dbus
      expat
      fontconfig
      freetype
      fuse3
      gdk-pixbuf
      glib
      gtk3
      icu
      libGL
      libappindicator-gtk3
      libdrm
      libglvnd
      libnotify
      libpulseaudio
      libunwind
      libusb1
      libuuid
      libxkbcommon
      libxml2
      mesa
      nspr
      nss
      pango
      pipewire
      systemd
      vulkan-loader
      xorg.libX11
      xorg.libXScrnSaver
      xorg.libXcomposite
      xorg.libXcursor
      xorg.libXdamage
      xorg.libXext
      xorg.libXfixes
      xorg.libXi
      xorg.libXrandr
      xorg.libXrender
      xorg.libXtst
      xorg.libxcb
      xorg.libxkbfile
      xorg.libxshmfence
    ];
  };
}
