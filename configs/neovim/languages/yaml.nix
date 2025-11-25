{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        yamlls.enable = true;
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
          "yamlls"
          "null-ls"
        ];

        settings = {
          yaml = {
            order = [
              "yamlls"
              "null-ls"
            ];
            sync = true;
          };
        };
      };
    };
  };
}
