--
-- Nvim LSP
local lsp_servers = { 'ansiblels', 'bashls', 'cssls', 'dockerls', 'eslint', 'html', 'jsonls', 'sumneko_lua', 'tsserver', 'volar' }
-- IMPORTANT: Mason must be set up before lspconfig
-- Mason to manage external tools like language servers
require('mason').setup()
require('mason-lspconfig').setup({
  ensure_installed = lsp_servers,
  automatic_installation = true,
})

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { buffer=bufnr }
  local wk = require('which-key')
  wk.register({
    g = {
      d = {vim.lsp.buf.definition, 'Go to definition', buffer = bufnr},
      D = {vim.lsp.buf.declaration, 'Go to declaration', buffer = bufnr},
      i = {vim.lsp.buf.implementation, 'Go to implementation', buffer = bufnr},
      r = {vim.lsp.buf.references, 'Go to references', buffer = bufnr},
      t = {vim.lsp.buf.type_definition, 'Go to type definition', buffer = bufnr},
    },
    K = {vim.lsp.buf.hover, 'Hover info', buffer = bufnr}
  })

  -- Format based on client. For TypeScript, always use eslint and prettier
  if (client.name == 'eslint' or client.name == 'tsserver') then
    -- EslintFixAll is provided by lsp-config, Prettier is provided by the prettier plugin
    wk.register({
      ['<leader>bf'] = {':EslintFixAll<CR>:Prettier<CR>', 'Format buffer', buffer = bufnr},
    })
  else
    wk.register({
      ['<leader>bf'] = {
        function() vim.lsp.buf.format {async = true} end,
        'Format buffer',
        buffer = bufnr
      },
    })
  end
  wk.register({
    c = {
      a = {vim.lsp.buf.code_action, 'Code actions', buffer = bufnr},
      k = {vim.lsp.buf.signature_help, 'Signature help', buffer = bufnr},
      r = {vim.lsp.buf.rename, 'Rename', buffer = bufnr}
    },
    w = {
      name = 'Workspace',
      a = {vim.lsp.buf.add_workspace_folder, 'Add workspace folder', buffer = bufnr},
      l = {
        function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
        'List workspace folders',
        buffer = bufnr
      },
      r = {vim.lsp.buf.remove_workspace_folder, 'Remove workspace folder', buffer = bufnr},
    },
  }, {prefix = '<leader>'})
end

local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

-- Add additional capabilities supported by nvim-cmp
local capabilities = require("cmp_nvim_lsp").default_capabilities()
local lspconfig = require('lspconfig')

-- Enable some language servers with the additional completion capabilities offered by nvim-cmp
for _, lsp in ipairs(lsp_servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
    settings = {
      json = {
        schemas = require('schemastore').json.schemas(),
        validate = { enable = true },
      }
    }
  }
end

-- Make `vim` a global in lua files
require'lspconfig'.sumneko_lua.setup {
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
    },
  },
}

--
-- LSP Saga
local saga = require("lspsaga")

saga.init_lsp_saga({
  code_action_lightbulb = {
    enable = false,
  }
})

--
-- NeoVim LSP server capabilities
local null_ls = require("null-ls")

null_ls.setup({
    sources = {
        null_ls.builtins.diagnostics.actionlint,
        null_ls.builtins.diagnostics.ansiblelint,
        null_ls.builtins.diagnostics.gitlint,
        null_ls.builtins.diagnostics.shellcheck,
        null_ls.builtins.code_actions.gitrebase,
        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.code_actions.shellcheck,
        null_ls.builtins.formatting.stylua,
        -- There are *so* many more ...
    },
})
