{ lib, pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        bashls.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        bash = [
          "shfmt"
        ];
        sh = [
          "shfmt"
        ];
      };
      formatters = {
        shfmt = {
          command = lib.getExe pkgs.shfmt;
        };
      };
    };
  };
}
