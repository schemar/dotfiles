{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        pyright.enable = true;
      };
    };
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          formatting = {
            black.enable = true;
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [
          "null-ls"
        ];

        settings = {
          erlang = {
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
