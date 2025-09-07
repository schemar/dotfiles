{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        bashls.enable = true;
        cssls.enable = true;
        elixirls = {
          enable = true;
          settings = {
            cmd = [ "elixir-ls" ];
          };
        };
        eslint.enable = true;
        gdscript.enable = true;
        just.enable = true;
        lua_ls.enable = true;
        nixd.enable = true;
        vtsls.enable = true;

        html = {
          enable = true;
          settings = {
            filetypes = [
              "html"
              "templ"
              "vue"
            ];
          };
        };

        jsonls.enable = true;
        yamlls.enable = true;
      };
    };
    plugins = {
      lsp.enable = true;
      # lspconfig.enable = true;
    };
  };
}
