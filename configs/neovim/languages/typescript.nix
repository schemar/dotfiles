{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        eslint.enable = true;
        vtsls = {
          enable = true;

          config = {
            settings = {
              # limit completions returned by vtsls to not break nvim performance.
              vtsls.experimental.completion.entriesLimit = 2000;

              vtsls.experimental.completion.enableServerSideFuzzyMatching = true;
              typescript.tsserver.maxTsServerMemory = 8192;
            };
          };
        };
      };
    };
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          formatting = {
            prettier = {
              enable = true;
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
