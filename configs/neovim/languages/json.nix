{ lib, pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        eslint.enable = true;
        jsonls.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        json = [
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
