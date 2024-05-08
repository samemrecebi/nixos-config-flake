{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: { 
  users.users.emrecebi = {
    home = "/Users/emrecebi";
  };
 
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    extra-platforms = x86_64-darwin aarch64-darwin
  '';

  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
}