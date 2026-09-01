{
  lib,
  pkgs,
  ...
}:
let
  full-terminal = command: [
    ":write-all"
    ":insert-output ${command}"
    ":redraw"
    ":reload-all"
  ];
  # Use ZSH with `-i` to use the same aliases that are configured for my
  # interactive shell.
  full-terminal-i = command: full-terminal "zsh -ic \"${command}\"";
  full-terminal-interactive = command: full-terminal "zsh -ic \"${command}\" >/dev/tty 2>&1";
in
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
      bash-language-server
      erlang-language-platform
      lua-language-server
      nixd
      nixfmt
      python313Packages.jedi # python
      ruff # python
      taplo # toml
      typescript-language-server
      ty # python
      vscode-langservers-extracted # html, css, json, eslint
      yamlfmt
      yaml-language-server
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
            C-g = full-terminal-i "lazygit";
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
        };

        file-picker.hidden = false;

        auto-save = {
          focus-lost = true;
          after-delay = {
            enable = true;
            timeout = 10000; # ms
          };
        };

        statusline = {
          left = [
            "mode"
            "file-name"
            "read-only-indicator"
            "file-modification-indicator"
          ];
          center = [ ];
          right = [
            "spinner"
            "spacer"
            "version-control"
            "spacer"
            "diagnostics"
            "separator"
            "selections"
            "register"
            "separator"
            "position"
            "position-percentage"
            "file-type"
          ];
        };
      };
    };
    languages = {
      language = [
        {
          name = "bash";
          auto-format = true;
          formatter = {
            command = "${lib.getExe pkgs.shfmt}";
          };
        }
        {
          name = "css";
          auto-format = true;
        }
        {
          name = "erlang";
          auto-format = true;
          formatter = {
            command = "${lib.getExe pkgs.erlfmt}";
            args = [ "-" ];
          };
        }
        {
          name = "html";
          auto-format = true;
        }
        {
          name = "javascript";
          auto-format = true;
          formatter = {
            command = "${lib.getExe pkgs.prettier}";
            args = [
              "--parser"
              "typescript"
            ];
          };
        }
        {
          name = "json";
          auto-format = true;
        }
        {
          name = "lua";
          auto-format = true;
        }
        {
          name = "nix";
          auto-format = true;
        }
        {
          name = "python";
          auto-format = true;
        }
        {
          name = "toml";
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
