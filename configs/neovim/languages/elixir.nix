{ pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        expert = {
          enable = true;
          package = pkgs.beamMinimal28Packages.expert;
        };
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        elixir = [
          "mix"
        ];
      };
    };

    plugins = {
      none-ls = {
        enable = true;
        sources = {
          diagnostics = {
            credo.enable = true;
          };
        };
      };
    };
  };

}
