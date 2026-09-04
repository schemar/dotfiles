{
  config,
  lib,
  pkgs,
  ...
}:
let
  miseInit = pkgs.runCommand "mise-init.zsh" { } ''
    ${lib.getExe config.programs.mise.package} activate zsh > "$out"
  '';
in
{
  programs.mise = {
    enable = true;
    enableZshIntegration = false;

    globalConfig = {
      settings = {
        auto_install = true;
      };
    };
  };

  programs.zsh.initContent = lib.mkOrder 900 ''
    source ${miseInit}
  '';
}
