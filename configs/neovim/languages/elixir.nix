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
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          diagnostics = {
            credo = {
              enable = true;
            };
          };
          formatting = {
            mix = {
              enable = true;
            };
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [
          "null-ls"
          "expert"
        ];

        settings = {
          elixir = {
            order = [
              "expert"
              "null-ls"
            ];
            sync = true;
          };
        };
      };
    };
  };

}
