{ pkgs, ... }:
{
  programs.zsh.shellAliases.h = "gh";

  programs.gh = {
    enable = true;
    extensions = with pkgs; [
      gh-stack
    ];
    settings = {
      git_protocol = "ssh";
      aliases = {
        s = "stack";
        sa = "stack add";
        sb = "stack bottom";
        sd = "stack down";
        sh = "stack push";
        so = "stack checkout";
        si = "stack init";
        sm = "stack modify";
        ss = "stack sync";
        st = "stack top";
        sp = "stack submit";
        sr = "stack rebase";
        su = "stack up";
        sv = "stack view";
        sw = "stack switch";
      };
    };
  };
}
