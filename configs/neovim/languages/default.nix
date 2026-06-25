{ ... }:
{
  imports = [
    ./elixir.nix
    ./erlang.nix
    ./gdscript.nix
    ./json.nix
    ./just.nix
    ./lua.nix
    ./markdown.nix
    ./nix.nix
    ./python.nix
    ./shell.nix
    ./typescript.nix
    ./web.nix
    ./xml.nix
    ./yaml.nix
  ];

  programs.nixvim = {
    plugins = {
      conform-nvim = {
        enable = true;
        settings = {
          formatters_by_ft = {
            "_" = [
              "trim_whitespace"
              "trim_newlines"
            ];
          };
          format_on_save = # Lua
            ''
              function(bufnr)
                if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
                  return
                end

                slow_format_filetypes = slow_format_filetypes or {}
                if slow_format_filetypes[vim.bo[bufnr].filetype] then
                  return
                end

                local function on_format(err)
                  if err and err:match("timeout$") then
                    slow_format_filetypes[vim.bo[bufnr].filetype] = true
                  end
                end

                return { timeout_ms = 200, lsp_fallback = true }, on_format
               end
            '';
          format_after_save = # Lua
            ''
              function(bufnr)
                if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
                  return
                end

                slow_format_filetypes = slow_format_filetypes or {}
                if not slow_format_filetypes[vim.bo[bufnr].filetype] then
                  return
                end

                return { lsp_fallback = true }
              end
            '';
        };
      };
      lspconfig.enable = true;
      schemastore.enable = true;
    };
  };

}
