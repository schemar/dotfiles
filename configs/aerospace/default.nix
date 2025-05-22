{ ... }:
{
  programs.aerospace = {
    enable = true;
    userSettings = {
      # Must be started manually once before this takes effect:
      start-at-login = true;

      gaps = {
        inner.horizontal = 7;
        inner.vertical = 7;
        outer.left = 7;
        outer.bottom = 7;
        outer.top = 7;
        outer.right = 7;
      };

      mode.main.binding = {
        ctrl-alt-cmd-shift-h = "focus left";
        ctrl-alt-cmd-shift-j = "focus down";
        ctrl-alt-cmd-shift-k = "focus up";
        ctrl-alt-cmd-shift-l = "focus right";

        ctrl-1 = "workspace 1";
        ctrl-2 = "workspace 2";
        ctrl-3 = "workspace 3";
        ctrl-4 = "workspace 4";
        ctrl-5 = "workspace 5";
        ctrl-6 = "workspace 6";
        ctrl-7 = "workspace 7";
        ctrl-8 = "workspace 8";
        ctrl-9 = "workspace 9";

        ctrl-shift-1 = "move-node-to-workspace 1";
        ctrl-shift-2 = "move-node-to-workspace 2";
        ctrl-shift-3 = "move-node-to-workspace 3";
        ctrl-shift-4 = "move-node-to-workspace 4";
        ctrl-shift-5 = "move-node-to-workspace 5";
        ctrl-shift-6 = "move-node-to-workspace 6";
        ctrl-shift-7 = "move-node-to-workspace 7";
        ctrl-shift-8 = "move-node-to-workspace 8";
        ctrl-shift-9 = "move-node-to-workspace 9";

        alt-tab = "workspace-back-and-forth";

        alt-shift-semicolon = "mode service";
      };

      mode.service.binding = {
        esc = [
          "reload-config"
          "mode main"
        ];
        r = [
          "flatten-workspace-tree"
          "mode main"
        ]; # reset layout
        f = [
          "layout floating tiling"
          "mode main"
        ]; # Toggle between floating and tiling layout
        backspace = [
          "close-all-windows-but-current"
          "mode main"
        ];
        minus = "resize smart -50";
        equal = "resize smart +50";
        alt-h = [
          "move left"
          "mode main"
        ];
        alt-j = [
          "move down"
          "mode main"
        ];
        alt-k = [
          "move up"
          "mode main"
        ];
        alt-l = [
          "move right"
          "mode main"
        ];
        alt-shift-h = [
          "join-with left"
          "mode main"
        ];
        alt-shift-j = [
          "join-with down"
          "mode main"
        ];
        alt-shift-k = [
          "join-with up"
          "mode main"
        ];
        alt-shift-l = [
          "join-with right"
          "mode main"
        ];
      };

      # Cannot auto-move Todoist as we cannot differentiate between the main app window and the quick entry pop-up.
      on-window-detected = [
        {
          check-further-callbacks = true;
          "if" = {
            app-id = "com.apple.systempreferences";
          };
          run = [
            "layout floating"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.github.wez.wezterm";
          };
          run = [
            "move-node-to-workspace 1"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.google.Chrome";
          };
          run = [
            "move-node-to-workspace 2"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.apple.Safari";
          };
          run = [
            "move-node-to-workspace 2"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.apple.Notes";
          };
          run = [
            "move-node-to-workspace 4"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "notion.id";
          };
          run = [
            "move-node-to-workspace 4"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.tinyspeck.slackmacgap";
          };
          run = [
            "move-node-to-workspace 5"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.apple.mail";
          };
          run = [
            "move-node-to-workspace 5"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.apple.Music";
          };
          run = [
            "move-node-to-workspace 6"
          ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "md.obsidian";
          };
          run = [
            "move-node-to-workspace 7"
          ];
        }
      ];
    };
  };
}
