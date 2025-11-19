{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        eslint.enable = true;
        jsonls.enable = true;
      };
    };
    plugins = {
      none-ls = {
        sources = {
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
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [
          "jsonls"
          "null-ls"
        ];

        settings = {
          json = {
            order = [
              "jsonls"
              "null-ls"
            ];
            sync = true;
          };
        };
      };
    };
  };
}
