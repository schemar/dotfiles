{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        nixd.enable = true;
      };
    };

    plugins.conform-nvim.settings = {
      formatters_by_ft = {
        nix = [
          "nixfmt"
        ];
      };
    };
  };
}
