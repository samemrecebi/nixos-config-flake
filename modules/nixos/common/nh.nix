{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    nh.enable =
      lib.mkEnableOption "Enable nh";
  };
  config = lib.mkIf config.nh.enable {
    programs.nh = {
      enable = true;
      clean.enable = false;
    };
  };
}
