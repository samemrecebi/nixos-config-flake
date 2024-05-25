{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    zsh.enable =
      lib.mkEnableOption "Enable zsh as default shell for users";
  };
  config = lib.mkIf config.zsh.enable {
    programs.zsh.enable = true;
    users.defaultUserShell = pkgs.zsh;
  };
}
