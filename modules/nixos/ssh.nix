{
  pkgs,
  options,
  config,
  ...
}: {
  #SSH
  programs.ssh = {
    startAgent = true;
  };
  # security with polkit
  security.polkit.enable = true;
  # security with gnome-kering
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.sddm.enableGnomeKeyring = true;
  services.openssh = {
    enable = true;
    settings = {
      X11Forwarding = true;
      # root user is used for remote deployment, so we need to allow it
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false; # disable password login
    };
    openFirewall = true;
  };
  systemd.user.services.add-ssh-key = {
    inherit (config.systemd.user.services.ssh-agent) unitConfig wantedBy;
    bindsTo = [
      "ssh-agent.service"
    ];
    environment.SSH_AUTH_SOCK = "/run/user/%U/ssh-agent";
    path = [
      options.programs.ssh.package.value
    ];
    script = "${options.programs.ssh.package.value}/bin/ssh-add";
    serviceConfig = {
      CapabilityBoundingSet = "";
      LockPersonality = true;
      NoNewPrivileges = true;
      ProtectClock = true;
      ProtectHostname = true;
      PrivateNetwork = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      RestrictAddressFamilies = "AF_UNIX";
      RestrictNamespaces = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = "~@clock @cpu-emulation @debug @module @mount @obsolete @privileged @raw-io @reboot @resources @swap";
      UMask = "0777";
    };
  };
}
