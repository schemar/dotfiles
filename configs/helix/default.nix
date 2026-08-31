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
          space.w = {
            s = "vsplit";
            C-s = "vsplit";
            v = "hsplit";
            C-v = "hsplit";
          };
        };
      };
      editor = {
        cursorline = true;
        color-modes = true;

        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };

        indent-guides = {
          render = true;
        };
      };
    };
    languages = {
      language-server.nixd.command = "${lib.getExe pkgs.nixd}";
      language-server.elp = {
        command = "${pkgs.erlang-language-platform}/bin/elp";
        args = [ "server" ];
      };
      language = [
        {
          name = "nix";
          auto-format = true;
          formatter = {
            command = "${lib.getExe pkgs.nixfmt}";
          };
        }
        {
          name = "erlang";
          auto-format = true;
          formatter = {
            command = "${lib.getExe pkgs.erlfmt}";
            args = [ "-" ];
          };
        }
      ];
    };
  };
}
