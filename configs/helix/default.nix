{ ... }:
{
  imports = [
    ./keys.nix
    ./languages
  ];

  # Does not work at the moment. Helix "does not find the file" ...
  # Copying manually for now :(
  # xdg.configFile."helix/themes/blueberry_peach_dark.toml" = {
  #   source = "${inputs.blueberry-peach}/ports/helix/blueberry_peach_dark.toml";
  # };

  programs.helix = {
    enable = true;
    defaultEditor = true;

    settings = {
      theme = "blueberry_peach_dark";
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
  };
}
