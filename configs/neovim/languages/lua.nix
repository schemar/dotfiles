{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        lua_ls.enable = true;
      };
    };
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          formatting = {
            stylua = {
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
          lua = {
            order = [ "null-ls" ];
            sync = true;
          };
        };
      };
    };
  };

}
