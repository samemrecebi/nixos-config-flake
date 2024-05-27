{
  config,
  lib,
  inputs,
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
      profiles.samemrecebi = {
        settings = {
          "browser.disableResetPrompt" = true;
          "browser.download.panel.shown" = true;
          "browser.download.useDownloadDir" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.startup.page" = 1;
          "browser.startup.homepage" = "about:home";
          "browser.search.defaultenginename" = "DuckDuckGo";
          "dom.security.https_only_mode" = true;
          "identity.fxaccounts.enabled" = false;
          "privacy.trackingprotection.enabled" = true;
          "signon.rememberSignons" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.server" = "data:";
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.newProfilePing.enabled" = false;
          "toolkit.telemetry.shutdownPingSender.enabled" = false;
          "toolkit.telemetry.updatePing.enabled" = false;
          "toolkit.telemetry.bhrPing.enabled" = false;
          "toolkit.telemetry.firstShutdownPing.enabled" = false;
          "toolkit.telemetry.coverage.opt-out" = true;
          "toolkit.coverage.opt-out" = true;
          "toolkit.coverage.endpoint.base." = "";
          "browser.ping-centre.telemetry" = false;
          "beacon.enabled" = false;
          "signon.autofillForms" = false;
          "browser.cache.disk.enable" = false;
          "browser.contentblocking.category" = "strict";
          "privacy.resistFingerprinting.block_mozAddonManager" = false;
          "extensions.pocket.enabled" = false;
          "app.shield.optoutstudies.enabled" = false;
          "app.normandy.enabled" = false;
          "app.normandy.api_url" = "";
          "extensions.getAddons.showPane" = false;
          "extensions.htmlaboutaddons.recommendations.enabled" = false;
          "browser.discovery.enabled" = false;
          "intl.accept_languages" = "en-US, en";
          "javascript.use_us_english_locale" = true;
          "network.IDN_show_punycode" = true;
          "geo.provider.network.url" = "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";
          "geo.provider.use_gpsd" = false;
          "geo.provider.use_geoclue" = false;
          "browser.region.network.url" = "";
          "browser.region.update.enabled" = false;
        };
        extensions = with inputs.firefox-addons.packages."x86_64-linux"; [
          bitwarden
          ublock-origin
          sponsorblock
        ];
      };
    };
    home.packages = with pkgs; [
      (pkgs.wrapFirefox (pkgs.firefox-unwrapped.override {pipewireSupport = true;}) {})
    ];
  };
}
