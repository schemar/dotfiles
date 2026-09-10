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
  };

  programs.zsh.initContent =
    lib.mkOrder 910 # bash
      ''
        if [[ $options[zle] = on ]]; then
          source ${fzfInit}

          export FZF_DEFAULT_COMMAND="fd --type f --hidden --exclude .git/"
          export FZF_CTRL_T_COMMAND="fd --type f --hidden --exclude .git/"
          export FZF_ALT_C_COMMAND="fd --type d --hidden --exclude .git/"

          # FZF_DEFAULT_OPTS Written in configs/fzf/default.nix instead of here
          # in order for theme to not append endlessly.
        fi
      '';
}
