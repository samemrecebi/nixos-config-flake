{pkgs, ...}: {
  # Enable and Configure Tailscale
  services.tailscale = {
    enable = true;
    package = pkgs.tailscale;
    useRoutingFeatures = "client";
    openFirewall = true;
  };
  networking.firewall.trustedInterfaces = ["tailscale0"];
}
