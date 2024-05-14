{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}:
# Universal python install (Im on a Mac I need it)
let
  python-with-global-packages = pkgs.python3.withPackages (ps:
    with ps; [
      pip
      botocore
    ]);
in {
  environment.systemPackages = with pkgs; [
    python-with-global-packages
    pyenv
  ];
}
