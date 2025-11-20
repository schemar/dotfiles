{ config, lib, ... }:
{
  options.lspServerHtmlFiletypes = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = ''
      Filetypes for the HTML LSP server.

      Use it in a language module that wants to enable HTML LSP support for specific filetypes.

      Example in web.nix:

      ```nix
      {
        lspServerHtmlFiletypes = [ "html" "templ" ];
      }
      ```
    '';
  };

  config.programs.nixvim.lsp.servers.html = {
    enable = true;
    config.filetypes = config.lspServerHtmlFiletypes;
  };
}
