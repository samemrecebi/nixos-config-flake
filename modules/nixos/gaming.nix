{pkgs, ...}: {
  # Extra Steam Packages
  environment.systemPackages = with pkgs; [
    mangohud
    protonup-ng
  ];

  # Steam
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    package = pkgs.steam.override {
      extraPkgs = pkgs:
        with pkgs; [
          keyutils
          libkrb5
          libpng
          libpulseaudio
          libvorbis
          stdenv.cc.cc.lib
          xorg.libXcursor
          xorg.libXi
          xorg.libXinerama
          xorg.libXScrnSaver

          # fix CJK fonts
          source-sans
          source-serif
          source-han-sans
          source-han-serif
        ];
    };
  };
  programs.gamemode.enable = true;
}
