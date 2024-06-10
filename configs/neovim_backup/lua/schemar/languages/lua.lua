local lsp_config = require("lspconfig")

lsp_config.lua_ls.setup({
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
  on_attach = require("schemar.languages.shared").on_attach,
  capabilities = require("schemar.languages.shared").capabilities,
})
lsp_config.efm.setup({
  on_attach = require("schemar.languages.shared").on_attach,
  init_options = { documentFormatting = true },
  filetypes = { "lua" },
})
