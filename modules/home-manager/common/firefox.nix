{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    firefox.enable =
      lib.mkEnableOption "Enable Firefox";
  };
  config = lib.mkIf config.firefox.enable {
    programs.firefox = {
      enable = true;
    };
    home.packages = with pkgs; [
      (pkgs.wrapFirefox (pkgs.firefox-unwrapped.override {pipewireSupport = true;}) {})
    ];
    xdg.mimeApps.defaultApplications = {
      "text/html" = ["firefox.desktop"];
      "text/xml" = ["firefox.desktop"];
      "x-scheme-handler/http" = ["firefox.desktop"];
      "x-scheme-handler/https" = ["firefox.desktop"];
    };
  };
}
