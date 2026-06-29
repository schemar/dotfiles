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
        alt-1 = "workspace 1";
        alt-2 = "workspace 2";
        alt-3 = "workspace 3";
        alt-4 = "workspace 4";
        alt-5 = "workspace 5";
        alt-6 = "workspace 6";
        alt-7 = "workspace 7";
        alt-8 = "workspace 8";
        alt-9 = "workspace 9";
        alt-0 = "workspace 10";

        alt-shift-1 = "move-node-to-workspace 1";
        alt-shift-2 = "move-node-to-workspace 2";
        alt-shift-3 = "move-node-to-workspace 3";
        alt-shift-4 = "move-node-to-workspace 4";
        alt-shift-5 = "move-node-to-workspace 5";
        alt-shift-6 = "move-node-to-workspace 6";
        alt-shift-7 = "move-node-to-workspace 7";
        alt-shift-8 = "move-node-to-workspace 8";
        alt-shift-9 = "move-node-to-workspace 9";
        alt-shift-0 = "move-node-to-workspace 10";

        alt-tab = "workspace-back-and-forth";
        alt-semicolon = "mode advanced";
      };

      mode.advanced.binding = {
        esc = "mode main";

        h = [
          "focus left"
          "mode main"
        ];
        j = [
          "focus down"
          "mode main"
        ];
        k = [
          "focus up"
          "mode main"
        ];
        l = [
          "focus right"
          "mode main"
        ];

        shift-h = [
          "move left"
          "mode main"
        ];
        shift-j = [
          "move down"
          "mode main"
        ];
        shift-k = [
          "move up"
          "mode main"
        ];
        shift-l = [
          "join-with right"
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

        r = "mode resize";
        space = "layout floating tiling"; # Toggle between floating and tiling

        c = [
          "reload-config"
          "mode main"
        ];
        f = [
          "flatten-workspace-tree"
          "mode main"
        ]; # reset layout
        backspace = [
          "close-all-windows-but-current"
          "mode main"
        ];
      };

      mode.resize.binding = {
        esc = [
          "mode main"
        ];
        h = "resize width -50";
        j = "resize height -50";
        k = "resize height +50";
        l = "resize width +50";
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
