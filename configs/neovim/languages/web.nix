{ lib, pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        cssls.enable = true;
        eslint.enable = true;
        html.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        html = [
          "prettier"
        ];
        css = [
          "prettier"
        ];
      };
      formatters = {
        prettier = {
          command = lib.getExe pkgs.prettier;
        };
      };
    };

    plugins = {
      none-ls = {
        enable = true;
        sources = {
          diagnostics = {
            stylelint = {
              enable = true;
            };
          };
        };
      };
    };
  };
}
