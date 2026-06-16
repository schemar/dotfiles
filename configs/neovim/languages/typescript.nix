{ lib, pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        eslint.enable = true;
        tsgo.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        typescript = [
          "prettier"
        ];
      };
      formatters = {
        prettier = {
          command = lib.getExe pkgs.prettier;
        };
      };
    };
  };
}
