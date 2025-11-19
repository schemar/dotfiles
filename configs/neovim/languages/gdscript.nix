{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        gdscript.enable = true;
      };
    };
    plugins = {
      none-ls = {
        sources = {
          diagnostics = {
            gdlint = {
              enable = true;
            };
          };
          formatting = {
            gdformat = {
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
          gdscript = {
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
