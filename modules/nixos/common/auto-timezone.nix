{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    auto-timezone.enable =
      lib.mkEnableOption "Enable automatic time zone detection";
  };
  config = lib.mkIf config.auto-timezone.enable {
    services.automatic-timezoned.enable = true;
  };
}
