{ lib, pkgs, ... }:
{
  programs.nixvim = {
    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        markdown = [
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
