{pkgs, ...}: {
  environment.systemPackages = with pkgs; [tailscale];
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
    openFirewall = true;
  };
  networking.firewall.trustedInterfaces = ["tailscale0"];
}
