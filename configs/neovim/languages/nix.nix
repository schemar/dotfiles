{ pkgs, ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        nixd.enable = true;
      };
    };
    plugins = {
      none-ls = {
        enable = true;
        sources = {
          formatting = {
            nixfmt = {
              enable = true;

              package = pkgs.nixfmt;
            };
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [
          "null-ls"
        ];

        settings = {
          nix = {
            order = [ "null-ls" ];
            sync = true;
          };
        };
      };
    };
  };

}
