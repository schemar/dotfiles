{ ... }:
let
  full-terminal = command: [
    ":write-all"
    ":insert-output ${command}"
    ":redraw"
    ":reload-all"
  ];
  # Use ZSH with `-i` to use the same aliases that are configured for my
  # interactive shell.
  full-terminal-interactive = command: full-terminal "zsh -ic \"${command}\" >/dev/tty 2>&1";
in
{
  programs.helix = {
    settings.keys = {
      normal = {
        C-w = {
          s = "vsplit";
          C-s = "vsplit";
          v = "hsplit";
          C-v = "hsplit";
        };
        C-g = {
          b = ":sh git blame -L %{selection_line_start},%{selection_line_end} %{buffer_name}";
          l = ":sh git log --max-count=1 --no-merges --oneline -L %{selection_line_start},%{selection_line_end}:%{buffer_name}";
          L = [
            ":sh echo \"%{selection_line_start},%{selection_line_end}:%{buffer_name}\" > /tmp/helix-git-log-selection"
            ":vsplit-new"
            ":insert-output git log -L $(cat /tmp/helix-git-log-selection)"
            ":set-language diff"
            ":sh rm -f /tmp/helix-git-log-selection"
          ];
          # Lazygit "integration":
          C-g = full-terminal-interactive "lazygit";
          s = {
            v = ":sh gh stack view";
            V = full-terminal-interactive "gh stack view";
            m = full-terminal-interactive "gh stack modify";
            u = [
              ":sh gh stack up"
              ":sh gh stack view"
            ];
            d = [
              ":sh gh stack down"
              ":sh gh stack view"
            ];
            s = ":sh gh stack sync";
            h = ":sh gh stack push";
          };
        };

        # Mark line and move with them up/down
        # https://github.com/helix-editor/helix/discussions/5764#discussioncomment-4840408
        C-j = [
          "extend_to_line_bounds"
          "delete_selection"
          "paste_after"
        ];
        C-k = [
          "extend_to_line_bounds"
          "delete_selection"
          "move_line_up"
          "paste_before"
        ];
        space = {
          # Yazi "integration":
          C-f = [
            ":sh rm -f /tmp/helix-yazi-selection-result"
            ":insert-output yazi --chooser-file=/tmp/helix-yazi-selection-result %{buffer_name}"
            ":sh printf \"\\x1b[?1049h\\x1b[?2004h\" > /dev/tty"
            ":open %sh{cat /tmp/helix-yazi-selection-result}"
            ":redraw"
            # If both Helix and Yazi have mouse support enabled, they will
            # conflict, you can use this trick to reset the mouse state when
            # exiting Yazi and returning to Helix (https://github.com/sxyazi/yazi/pull/2461):
            ":set mouse false"
            ":set mouse true"
          ];
          # Other file:
          o = [
            ":sh rm -f /tmp/helix-other-file-selection-result"
            ":insert-output other-file %{buffer_name} /tmp/helix-other-file-selection-result 1>/dev/tty 2>&1"
            ":open %sh{cat /tmp/helix-other-file-selection-result}"
            ":redraw"
          ];
          w = {
            s = "vsplit";
            C-s = "vsplit";
            v = "hsplit";
            C-v = "hsplit";
          };
        };
      };
    };
  };
}
