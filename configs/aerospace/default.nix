{ ... }:
{
  programs.aerospace = {
    enable = true;
    # Automatically start at startup
    launchd.enable = true;

    settings = {
      gaps = {
        inner.horizontal = 6;
        inner.vertical = 6;
        outer.left = 6;
        outer.bottom = 6;
        outer.top = 0;
        outer.right = 6;
      };

      mode.main.binding = {
        ctrl-alt-h = "focus left";
        ctrl-alt-j = "focus down";
        ctrl-alt-k = "focus up";
        ctrl-alt-l = "focus right";

        ctrl-alt-shift-h = "move left";
        ctrl-alt-shift-j = "move down";
        ctrl-alt-shift-k = "move up";
        ctrl-alt-shift-l = "move right";

        ctrl-alt-cmd-shift-h = "join-with left";
        ctrl-alt-cmd-shift-j = "join-with down";
        ctrl-alt-cmd-shift-k = "join-with up";
        ctrl-alt-cmd-shift-l = "join-with right";

        ctrl-alt-1 = "workspace 1";
        ctrl-alt-2 = "workspace 2";
        ctrl-alt-3 = "workspace 3";
        ctrl-alt-4 = "workspace 4";
        ctrl-alt-5 = "workspace 5";
        ctrl-alt-6 = "workspace 6";
        ctrl-alt-7 = "workspace 7";
        ctrl-alt-8 = "workspace 8";
        ctrl-alt-9 = "workspace 9";
        ctrl-alt-0 = "workspace 10";

        ctrl-alt-shift-1 = "move-node-to-workspace 1";
        ctrl-alt-shift-2 = "move-node-to-workspace 2";
        ctrl-alt-shift-3 = "move-node-to-workspace 3";
        ctrl-alt-shift-4 = "move-node-to-workspace 4";
        ctrl-alt-shift-5 = "move-node-to-workspace 5";
        ctrl-alt-shift-6 = "move-node-to-workspace 6";
        ctrl-alt-shift-7 = "move-node-to-workspace 7";
        ctrl-alt-shift-8 = "move-node-to-workspace 8";
        ctrl-alt-shift-9 = "move-node-to-workspace 9";
        ctrl-alt-shift-0 = "move-node-to-workspace 10";

        alt-tab = "workspace-back-and-forth";

        ctrl-alt-semicolon = "mode service";
        ctrl-alt-r = "mode resize";
        ctrl-alt-space = "layout floating tiling"; # Toggle between floating and tiling
      };

      mode.resize.binding = {
        h = "resize smart -50";
        l = "resize smart +50";
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
        backspace = [
          "close-all-windows-but-current"
          "mode main"
        ];
      };

      # Cannot auto-move Todoist as we cannot differentiate between the main app window and the quick entry pop-up.
      # Find the app IDs with `aerospace list-apps`
      on-window-detected = [
        {
          check-further-callbacks = true;
          "if" = {
            app-id = "com.apple.systempreferences";
          };
          run = [ "layout floating" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.mitchellh.ghostty";
          };
          run = [ "move-node-to-workspace 1" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.github.wez.wezterm";
          };
          run = [ "move-node-to-workspace 1" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.google.Chrome";
          };
          run = [ "move-node-to-workspace 2" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.apple.Safari";
          };
          run = [ "move-node-to-workspace 2" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.vivaldi.Vivaldi";
          };
          run = [ "move-node-to-workspace 2" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.apple.Notes";
          };
          run = [ "move-node-to-workspace 4" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "notion.id";
          };
          run = [ "move-node-to-workspace 4" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.tinyspeck.slackmacgap";
          };
          run = [ "move-node-to-workspace 5" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.apple.mail";
          };
          run = [ "move-node-to-workspace 5" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "com.apple.Music";
          };
          run = [ "move-node-to-workspace 6" ];
        }
        {
          check-further-callbacks = false;
          "if" = {
            app-id = "md.obsidian";
          };
          run = [ "move-node-to-workspace 7" ];
        }
      ];
    };
  };
}
