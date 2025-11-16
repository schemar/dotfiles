{ pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        elixirls = {
          enable = true;
          config = {
            cmd = [ "elixir-ls" ];
          };
        };
      };
    };
    plugins = {
      none-ls = {
        sources = {
          diagnostics = {
            credo = {
              enable = true;
            };
          };
          formatting = {
            mix = {
              enable = true;
            };
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [
          "null-ls"
          "elixirls"
        ];

        settings = {
          elixir = {
            order = [
              "elixir-ls"
              "null-ls"
            ];
            sync = true;
          };
        };
      };
    };
  };

}
