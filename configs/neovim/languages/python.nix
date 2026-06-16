{ lib, pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        pyright.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        python = [
          "black"
        ];
      };
      formatters = {
        black = {
          command = lib.getExe pkgs.black;
        };
      };
    };
  };
}
