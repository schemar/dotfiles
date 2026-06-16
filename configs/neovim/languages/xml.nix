{ lib, pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        lemminx.enable = true; # XML LSP
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        xml = [
          "xmlformatter"
        ];
      };
      formatters = {
        xmlformatter = {
          command = lib.getExe pkgs.xmlformat;
        };
      };
    };
  };
}
