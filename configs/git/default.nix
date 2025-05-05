{ ... }: {
  programs.git = {
    enable = true;

    aliases = {
      a = "add -A";
      b = "branch";
      bd = "branch -d";
      c = "commit";
      d = "diff";
      ds = "diff --staged";
      e = "restore";
      h = "push";
      l = "log --pretty=short --max-count=10";
      n = "clean -fd src";
      o = "switch";
      p = "pull -p";
      r = "rebase";
      s = "status";
    };

    delta = {
      # We use our own git pager setting to have dynamic theme support:
      # (system package is enabled)
      enable = false;
      options = {
        line-numbers = true;
      };
    };

    includes = [
      {
        path = "~/.gitconfigprivate";
        condition =  "gitdir:*";
      }
      {
        path = "~/.gitconfigafilio";
        condition =   "gitdir:afilio*/";
      }
    ];

    extraConfig = {
      branch = {
        sort = "-committerdate";
      };
      column = {
        ui = "auto";
      };
      core = {
        # Enable dynamic delta syntax theme:
        pager = "delta --features syntax-theme-`$HOME/.config/current_theme`";
      };
      "delta \"syntax-theme-light\"" = {
        syntax-theme = "blueberry_peach_light";
      };
      "delta \"syntax-theme-dark\"" = {
        syntax-theme = "blueberry_peach_light";
      };
      diff = {
        algorithm = "histogram";
        colorMoved = "plain";
        mnemonicPrefix = "true";
        renames = "true";
      };
      fetch = {
        prune = "true";
        pruneTags = "true";
        all = "true";
      };
      github = {
        user = "schemar";
      };
      help = {
        autocorrect = "prompt";
      };
      init = {
        defaultBranch = "main";
      };
      log = {
        follow = "true";
      };
      merge = {
        tool = "nvimdiff";
        conflictstyle = "zdiff3";
      };
      mergetool = {
        keepBackup = "false";
      };
      pull = {
        rebase = "true";
      };
      push = {
        default = "simple";
        autoSetupRemote = "true";
        followTags = "true";
      };
      rebase = {
        autoSquash = "true";
        autoStash = "true";
        updateRefs = "true";
      };
      rerere = {
        enabled = "true";
        autoupdate = "true";
      };
      tag = {
        sort = "version:refname";
      };
    };
  };
}
