{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    automatic-timezoned.enable =
      lib.mkEnableOption "Enable automatic time zone detection";
  };
  config = lib.mkIf config.automatic-timezoned.enable {
    services.automatic-timezoned.enable = true;
  };
}
