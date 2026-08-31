{
  lib,
  pkgs,
  ...
}:
{
  # Does not work at the moment. Helix "does not find the file" ...
  # Copying manually for now :(
  # xdg.configFile."helix/themes/blueberry_peach_dark.toml" = {
  #   source = "${inputs.blueberry-peach}/ports/helix/blueberry_peach_dark.toml";
  # };
  programs.helix = {
    enable = true;
    defaultEditor = true;

    extraPackages = with pkgs; [
      erlang-language-platform
      nixd
      nixfmt
      taplo # toml
      yaml-language-server
      yamlfmt
    ];

    settings = {
      theme = "blueberry_peach_dark";
      keys = {
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
            C-g = [
              ":write-all"
              # Use ZSH with `-i` (interactive) to use the same aliases that are
              # configured for my interactive shell.
              ":insert-output zsh -ic lazygit"
              ":redraw"
              ":reload-all"
            ];
          };
          space = {
            # Yazi "integration":
            C-f = [
              ":sh rm -f /tmp/helix-yazi-selection-result"
              ":insert-output yazi --chooser-file=/tmp/helix-yazi-selection-result"
              ":sh printf \"\\x1b[?1049h\\x1b[?2004h\" > /dev/tty"
              ":open %sh{cat /tmp/helix-yazi-selection-result}"
              ":redraw"
              # If both Helix and Yazi have mouse support enabled, they will
              # conflict, you can use this trick to reset the mouse state when
              # exiting Yazi and returning to Helix (https://github.com/sxyazi/yazi/pull/2461):
              ":set mouse false"
              ":set mouse true"
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
      editor = {
        # Use system clipboard
        default-yank-register = "+";

        # These may not get picked up properly in tmux
        true-color = true;
        undercurl = true;

        rulers = [
          80
          120
        ];

        color-modes = true;
        trim-trailing-whitespace = true;
        popup-border = "all";

        indent-guides = {
          render = true;
        };

        end-of-line-diagnostics = "error";
        inline-diagnostics = {
          cursor-line = "info";
          other-lines = "error";
        };
      };
    };
    languages = {
      language = [
        {
          name = "erlang";
          auto-format = true;
          formatter = {
            command = "${lib.getExe pkgs.erlfmt}";
            args = [ "-" ];
          };
        }
        {
          name = "nix";
          auto-format = true;
        }
        {
          name = "yaml";
          auto-format = true;
        }
      ];
      grammar = [
        {
          name = "erlang";
          source = {
            git = "https://github.com/WhatsApp/tree-sitter-erlang";
            rev = "67e7f7f05baf492ca2a7c0d9538761b242d33d95";
          };
        }
      ];
    };
  };
}
