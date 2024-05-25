{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    tailscale.enable =
      lib.mkEnableOption "Set up tailscale for mesh VPN access";
  };
  config = lib.mkIf config.tailscale.enable {
    environment.systemPackages = with pkgs; [tailscale];
    services.tailscale = {
      enable = true;
      useRoutingFeatures = "client";
      openFirewall = true;
    };
    networking.firewall.trustedInterfaces = ["tailscale0"];
  };
}
