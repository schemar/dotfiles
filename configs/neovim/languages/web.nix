{ ... }:
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
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          diagnostics = {
            stylelint = {
              enable = true;
            };
          };
          formatting = {
            prettier = {
              enable = true;
            };
            stylelint = {
              enable = true;
            };
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [
          "cssls"
          "eslint"
          "null-ls"
        ];

        settings = {
          html = {
            order = [ "null-ls" ];
            sync = true;
            # Format HTML only with prettier.
            exclude = [ "html" ];
          };
          scss = {
            order = [
              "cssls"
              "null-ls"
            ];
            sync = true;
          };
        };
      };
    };
  };
}
