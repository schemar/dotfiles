{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        elp = {
          enable = true;
        };
      };
    };
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          formatting = {
            erlfmt = {
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
