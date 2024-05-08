{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
services.nix-daemon.enable = true;
 
  users.users.emrecebi = {
    home = "/Users/emrecebi";
  };
 
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    extra-platforms = x86_64-darwin aarch64-darwin
  '';
}