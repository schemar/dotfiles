{ lib, pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        yamlls.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        yaml = [
          "yamlfmt"
        ];
      };
      formatters = {
        yamlfmt = {
          command = lib.getExe pkgs.yamlfmt;
        };
      };
    };
  };
}
