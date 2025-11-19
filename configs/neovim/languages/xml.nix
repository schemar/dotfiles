{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        lemminx.enable = true; # XML LSP
      };
    };
    plugins = {
      lsp-format = {
        lspServersToEnable = [
          "lemminx"
        ];

        settings = {
          xml = {
            order = [
              "lemminx"
            ];
            sync = true;
          };
        };
      };
    };
  };
}
