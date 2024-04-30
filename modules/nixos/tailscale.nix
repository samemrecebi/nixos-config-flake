{pkgs, self, ...} :
{
  services.tailscale.enable = true;
  services.tailscale.useRoutingFeatures = "client";
  networking.firewall.trustedInterfaces = ["tailscale0"];
}