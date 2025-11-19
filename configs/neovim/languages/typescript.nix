{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        eslint.enable = true;
        vtsls.enable = true;
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
          "eslint"
          "null-ls"
        ];

        settings = {
          javascript = {
            order = [
              "eslint"
              "null-ls"
            ];
            sync = true;
            # Format JS only with eslint and prettier.
            exclude = [ "vtsls" ];
          };
          typescript = {
            order = [
              "eslint"
              "null-ls"
            ];
            sync = true;
            # Format TS only with eslint and prettier.
            exclude = [ "vtsls" ];
          };
        };
      };
    };
  };
}
