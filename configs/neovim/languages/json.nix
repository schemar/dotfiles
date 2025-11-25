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
