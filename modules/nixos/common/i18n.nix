{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    i18n.enable =
      lib.mkEnableOption "Set up internationalization support";
  };
  config = lib.mkIf config.i18n.enable {
    i18n.supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "nl_NL.UTF-8/UTF-8"
      "tr_TR.UTF-8/UTF-8"
    ];
    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
      LC_ADDRESS = "nl_NL.UTF-8";
      LC_IDENTIFICATION = "nl_NL.UTF-8";
      LC_MEASUREMENT = "tr_TR.UTF-8";
      LC_MONETARY = "nl_NL.UTF-8";
      LC_NAME = "tr_TR.UTF-8";
      LC_NUMERIC = "nl_NL.UTF-8";
      LC_PAPER = "nl_NL.UTF-8";
      LC_TELEPHONE = "nl_NL.UTF-8";
      LC_TIME = "tr_TR.UTF-8";
    };
  };
}
