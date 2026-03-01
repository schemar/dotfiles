{ ... }:
{
  programs.nixvim = {
    lsp.servers.beancount.enable = true;
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          diagnostics = {
            beancount.enable = true;
          };
          formatting = {
            beancount.enable = true;
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [ "none-ls" ];

        settings.beancount = {
          order = [ "none-ls" ];
          sync = true;
        };
      };
    };
  };
}
