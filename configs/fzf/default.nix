{
  config,
  lib,
  pkgs,
  ...
}:
let
  fzfInit = pkgs.runCommand "fzf-init.zsh" { } ''
    ${lib.getExe config.programs.fzf.package} --zsh > "$out"
  '';
in
{
  programs.fzf = {
    enable = true;
    enableZshIntegration = false;

    defaultCommand = "fd --type f --hidden --exclude .git/";
  };

  programs.zsh.initContent = lib.mkOrder 910 ''
    if [[ $options[zle] = on ]]; then
      source ${fzfInit}
    fi
  '';
}
