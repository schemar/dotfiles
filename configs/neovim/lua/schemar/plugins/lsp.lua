--
-- Nvim LSP
-- Disabling volar for now, as it does not pick up the types in the project.
local lsp_servers = {
  'ansiblels', 'bashls', 'cssls', 'dockerls', 'eslint', 'html', 'jsonls',
  'sumneko_lua', 'tsserver',
}

-- IMPORTANT: Mason must be set up before lspconfig
-- Mason to manage external tools like language servers
require('mason').setup()
require('mason-lspconfig').setup({
  ensure_installed = lsp_servers,
  automatic_installation = true,
})

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local augroup = vim.api.nvim_create_augroup('LspFormatting', {})
local format_timeout_ms = 2000
local navic = require('nvim-navic')
local icons = require('schemar.icons').kind
navic.setup({
  icons = {
    Array = icons.Array .. ' ',
    Boolean = icons.Boolean,
    Class = icons.Class .. ' ',
    Color = icons.Color .. ' ',
    Constant = icons.Constant .. ' ',
    Constructor = icons.Constructor .. ' ',
    Enum = icons.Enum .. ' ',
    EnumMember = icons.EnumMember .. ' ',
    Event = icons.Event .. ' ',
    Field = icons.Field .. ' ',
    File = icons.File .. ' ',
    Folder = icons.Folder .. ' ',
    Function = icons.Function .. ' ',
    Interface = icons.Interface .. ' ',
    Key = icons.Key .. ' ',
    Keyword = icons.Keyword .. ' ',
    Method = icons.Method .. ' ',
    Module = icons.Module .. ' ',
    Namespace = icons.Namespace .. ' ',
    Null = icons.Null .. ' ',
    Number = icons.Number .. ' ',
    Object = icons.Object .. ' ',
    Operator = icons.Operator .. ' ',
    Package = icons.Package .. ' ',
    Property = icons.Property .. ' ',
    Reference = icons.Reference .. ' ',
    Snippet = icons.Snippet .. ' ',
    String = icons.String .. ' ',
    Struct = icons.Struct .. ' ',
    Text = icons.Text .. ' ',
    TypeParameter = icons.TypeParameter .. ' ',
    Unit = icons.Unit .. ' ',
    Value = icons.Value .. ' ',
    Variable = icons.Variable .. ' ',
  },
  highlight = true,
  separator = ' ' .. require('schemar.icons').ui.ChevronShortRight .. ' ',
  depth_limit = 0,
  depth_limit_indicator = '..',
  safe_output = true,
})

-- LSP custom function when client attaches to buffer.
local on_attach = function(client, bufnr)
  -- Auto-format on save
  if client.supports_method('textDocument/formatting') then
    vim.api.nvim_clear_autocmds({group = augroup, buffer = bufnr})
    vim.api.nvim_create_autocmd('BufWritePre', {
      group = augroup,
      buffer = bufnr,
      callback = function()
        vim.lsp.buf.format({timeout_ms = format_timeout_ms})
      end,
    })
  end

  -- Attach navic for winbar breadcrumbs
  if client.server_capabilities.documentSymbolProvider then
    navic.attach(client, bufnr)
  end

  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local wk = require('which-key')

  -- Without prefix:
  wk.register({
    ['['] = {
      d = {
        ':Lspsaga diagnostic_jump_prev<CR>',
        'Previous diagnostic',
        buffer = bufnr,
      },
      D = {
        function()
          require('lspsaga.diagnostic').goto_prev({
            severity = vim.diagnostic.severity.Error,
          })
        end,
        'Previous error',
        buffer = bufnr,
      },
    },
    [']'] = {
      d = {
        ':Lspsaga diagnostic_jump_next<CR>',
        'Next diagnostic',
        buffer = bufnr,
      },
      D = {
        function()
          require('lspsaga.diagnostic').goto_next({
            severity = vim.diagnostic.severity.Error,
          })
        end,
        'Next error',
        buffer = bufnr,
      },
    },
    g = {
      d = {vim.lsp.buf.definition, 'Go to definition', buffer = bufnr},
      D = {':Lspsaga peek_definition<CR>', 'Peek definition', buffer = bufnr},
      h = {':Lspsaga lsp_finder<CR>', 'Find symbol ...', buffer = bufnr},
      i = {vim.lsp.buf.implementation, 'Go to implementation', buffer = bufnr},
      r = {
        ':TroubleToggle lsp_references<CR>',
        'Go to references',
        buffer = bufnr,
      },
      t = {vim.lsp.buf.type_definition, 'Go to type definition', buffer = bufnr},
    },
    K = {':Lspsaga hover_doc<CR>', 'Hover doc', buffer = bufnr},
  })

  -- With leader prefix:
  wk.register({
    b = {
      f = {
        function()
          vim.lsp.buf.format({async = true, timeout_ms = format_timeout_ms})
        end,
        'Format buffer',
        buffer = bufnr,
      },
    },
    c = {
      a = {':Lspsaga code_action<CR>', 'Code actions', buffer = bufnr},
      e = {
        ':Lspsaga show_line_diagnostics<CR>',
        'Line diagnostics',
        buffer = bufnr,
      },
      k = {vim.lsp.buf.signature_help, 'Signature help', buffer = bufnr},
      o = {':Lspsaga outline_toggle<CR>', 'Toggle outline', buffer = bufnr},
      r = {':Lspsaga rename <CR>', 'Rename', buffer = bufnr},
    },
    ['<tab>'] = {
      name = 'Workspace',
      a = {
        vim.lsp.buf.add_workspace_folder,
        'Add workspace folder',
        buffer = bufnr,
      },
      l = {
        function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end,
        'List workspace folders',
        buffer = bufnr,
      },
      r = {
        vim.lsp.buf.remove_workspace_folder,
        'Remove workspace folder',
        buffer = bufnr,
      },
    },
  }, {prefix = '<leader>'})

  -- Visual mode with leader prefix:
  wk.register({c = {a = {':Lspsaga code_action<CR>', buffer = bufnr}}},
              {prefix = '<leader>', mode = 'v'})
end

local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

-- Add additional capabilities supported by nvim-cmp
local capabilities = require('cmp_nvim_lsp').default_capabilities()
local lspconfig = require('lspconfig')

-- Enable some language servers with the additional completion capabilities offered by nvim-cmp
for _, lsp in ipairs(lsp_servers) do
  lspconfig[lsp].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
    settings = {
      json = {
        schemas = require('schemastore').json.schemas(),
        validate = {enable = true},
      },
    },
  })
end

-- This is for diagnostic signs on the line number column.
-- Use this to beautify the plain E W signs.
local signs = require('schemar.icons').diagnostics
for type, icon in pairs(signs) do
  local hl = 'DiagnosticSign' .. type
  vim.fn.sign_define(hl, {text = icon, texthl = hl, numhl = hl})
end

-- Make `vim` a global in lua files
require('lspconfig').sumneko_lua.setup({
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
    },
  },
})

--
-- LSP Saga
local saga = require('lspsaga')

saga.init_lsp_saga({
  code_action_lightbulb = {enable = false},
  finder_action_keys = {
    open = {'o', '<CR>'},
    vsplit = 's',
    split = 'v',
    tabe = 't',
    quit = {'q', '<ESC>'},
  },
  code_action_keys = {quit = 'q', exec = '<CR>'},
  definition_action_keys = {
    edit = '<C-c>o',
    vsplit = '<C-c>s',
    split = '<C-c>v',
    tabe = '<C-c>t',
    quit = 'q',
  },
  rename_action_quit = '<C-c>',
})

--
-- NeoVim LSP server capabilities
local null_ls = require('null-ls')

null_ls.setup({
  sources = {
    null_ls.builtins.diagnostics.actionlint,
    null_ls.builtins.diagnostics.ansiblelint,
    null_ls.builtins.diagnostics.eslint_d,
    null_ls.builtins.diagnostics.jsonlint,
    -- null_ls.builtins.diagnostics.luacheck, -- No binary, yet
    -- null_ls.builtins.diagnostics.markdownlint, -- Disabled, too noisy
    null_ls.builtins.diagnostics.shellcheck, null_ls.builtins.diagnostics.tidy,
    -- null_ls.builtins.diagnostics.yamllint, -- Disabled, too noisy
    null_ls.builtins.code_actions.eslint_d,
    null_ls.builtins.code_actions.gitrebase,
    null_ls.builtins.code_actions.gitsigns,
    null_ls.builtins.code_actions.refactoring,
    null_ls.builtins.code_actions.shellcheck,
    null_ls.builtins.formatting.eslint_d, null_ls.builtins.formatting.fixjson,
    null_ls.builtins.formatting.jq, null_ls.builtins.formatting.markdownlint,
    null_ls.builtins.formatting.prettier, null_ls.builtins.formatting.shfmt,
    null_ls.builtins.formatting.stylua, null_ls.builtins.formatting.tidy,
    null_ls.builtins.formatting.yamlfmt,
    -- There are *so* many more ...
  },
})

--
-- LSP signature
require('lsp_signature').setup({
  hint_prefix = require('schemar.icons').ui.ChevronShortRight .. ' ',
  floating_window = false, -- Virtual text for arg names and types only
})
