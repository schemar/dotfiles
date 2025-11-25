{ ... }:
{
  lspServerHtmlFiletypes = [
    "vue"
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
              settings = {
                extra_filetypes = [ "vue" ];
              };
            };
          };
          formatting = {
            prettier = {
              enable = true;
            };
            stylelint = {
              enable = true;
              settings = {
                extra_filetypes = [ "vue" ];
              };
            };
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [
          "null-ls"
        ];

        settings = {
          vue = {
            order = [ "null-ls" ];
            sync = true;
            # Format only with stylelint and prettier.
            exclude = [ "html" ];
          };
        };
      };
    };
  };
}
