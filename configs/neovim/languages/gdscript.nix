{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        gdscript.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        gdscript = [
          "gdformat"
        ];
      };
    };

    plugins = {
      none-ls = {
        enable = true;
        sources = {
          diagnostics = {
            gdlint.enable = true;
          };
        };
      };
    };
  };
}
