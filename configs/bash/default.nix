{ lib, npmAlias, ... }:
{
  programs.bash = {
    enable = true;

    shellAliases = {
      npm = lib.mkIf (npmAlias != null) npmAlias;
    };
  };
}
