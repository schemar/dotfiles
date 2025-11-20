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
      };
    };
    plugins = {
      none-ls = {
        sources = {
          diagnostics = {
            stylelint = {
              enable = true;
            };
          };
          formatting = {
            prettier = {
              enable = true;
              settings = {
                # Always format open files, even if they are ignored.
                # Setting `ignore-path` to empty string means "ignore nothing".
                # By default, prettier will ignore files in .gitignore and .prettierignore.
                extra_args = [
                  "--ignore-path"
                  ""
                ];
              };
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
