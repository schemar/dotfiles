{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        just.enable = true;
      };
    };
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          formatting = {
            just = {
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
          just = {
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
