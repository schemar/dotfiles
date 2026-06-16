{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        just.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        just = [
          "just"
        ];
      };
    };
  };
}
