{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        marksman.enable = true;
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
          "null-ls"
        ];

        settings = {
          markdown = {
            order = [
              "null-ls"
            ];
            sync = true;
          };
        };
      };
    };
  };

}
