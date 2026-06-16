{ lib, pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        elp = {
          enable = true;
        };
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        erlang = [
          "erlfmt"
        ];
      };
      formatters = {
        erlfmt = {
          command = lib.getExe pkgs.erlfmt;
        };
      };
    };
  };
}
