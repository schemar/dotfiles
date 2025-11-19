{ ... }:
{
  programs.nixvim = {
    plugins = {
      none-ls = {
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
