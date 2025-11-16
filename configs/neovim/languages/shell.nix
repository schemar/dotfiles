{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        bashls.enable = true;
      };
    };
    plugins = {
      lsp-format = {
        lspServersToEnable = [
          "bashls"
        ];

        settings = {
          sh = {
            order = [ "bashls" ];
            sync = true;
          };
          zsh = {
            order = [ "bashls" ];
            sync = true;
          };

        };
      };
    };
  };

}
