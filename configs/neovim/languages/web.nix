{ lib, pkgs, ... }:
{
  lspServerHtmlFiletypes = [
    "html"
    "templ"
  ];

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
          "html_beautify"
        ];
        css = [
          "css_beautify"
        ];
      };
      formatters = {
        html_beautify = {
          command = lib.getExe pkgs.js-beautify;
        };
        css_beautify = {
          command = lib.getExe pkgs.js-beautify;
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
