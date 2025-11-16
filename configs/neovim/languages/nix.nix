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
        sources = {
          formatting = {
            nixfmt = {
              enable = true;

              package = pkgs.nixfmt-rfc-style;
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
