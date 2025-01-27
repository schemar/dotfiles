local term_command = "split | terminal "

local term = function(command)
  vim.cmd(term_command .. command)
end

local term_and_edit = function(command)
  local prev_bufnr = vim.api.nvim_get_current_buf()

  -- Open a terminal and run the command
  term(command)
  local term_bufnr = vim.api.nvim_get_current_buf()

  -- React when the terminal command finishes
  vim.api.nvim_create_autocmd("TermClose", {
    buffer = term_bufnr,
    once = true,
    callback = function()
      -- Wait for user input before removing the buffer
      vim.api.nvim_echo({ { "\nPress <CR> to close the terminal", "WarningMsg" } }, false, {})
      -- Force close the current (terminal) window
      vim.api.nvim_win_close(0, true)
      -- Force close the terminal buffer
      vim.api.nvim_buf_delete(term_bufnr, { force = true })
      -- Go back to the previous buffer
      vim.api.nvim_set_current_buf(prev_bufnr)
      -- Call `edit` to re-load file content
      vim.cmd("edit")
    end,
  })
end

return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  keys = {
    {
      "<leader>?",
      function()
        require("which-key").show({ global = false })
      end,
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
  config = function()
    local wk = require("which-key")
    wk.setup({
      preset = "helix",
      icons = {
        mappings = false,
      },
    })
    wk.add({
      { "<c-w>", group = "Window" },
      {
        "<c-w>s",
        "<cmd>vsplit<cr>",
        desc = "Split window",
      },
      {
        "<m-s>",
        "<cmd>vsplit<cr>",
        desc = "Split window",
      },
      {
        "<c-w>v",
        "<cmd>split<cr>",
        desc = "Split window verticallly",
      },
      { "<leader>", group = "Leader" },
      { "<leader>b", desc = "Buffers" },
      { "<leader>g", desc = "Git" },
      { "<leader>a", group = "Graphite" },
      {
        "<leader>al",
        "<cmd>!gt ls<cr>",
        desc = "List stacks",
      },
      {
        "<leader>ac",
        function()
          term("gt create")
        end,
        desc = "Create",
      },
      {
        "<leader>as",
        function()
          term_and_edit("gt sync")
        end,
        desc = "Sync",
      },
      {
        "<leader>ap",
        function()
          term("gt submit --no-edit")
        end,
        desc = "Submit",
      },
      {
        "<leader>aP",
        function()
          term("gt submit --no-edit --publish")
        end,
        desc = "Submit (publish)",
      },
      {
        "<leader>am",
        function()
          term("gt modify")
        end,
        desc = "Modify",
      },
      {
        "<leader>ao",
        function()
          term_and_edit("gt checkout")
        end,
        desc = "Checkout",
      },
      {
        "<leader>au",
        "<cmd>!gt up --no-interactive --quiet<cr><cmd>edit<cr>",
        desc = "Up",
      },
      {
        "<leader>ad",
        "<cmd>!gt down --no-interactive --quiet<cr><cmd>edit<cr>",
        desc = "Down",
      },
      { "<leader>l", group = "Code" },
      {
        "<leader>la",
        vim.lsp.buf.code_action,
        desc = "Code actions",
      },
      {
        "<leader>le",
        vim.diagnostic.open_float,
        desc = "Floating diagnostic",
      },
      { "<leader>lr", vim.lsp.buf.rename, desc = "Rename" },
      { "<leader>t", desc = "Todos" },
    })
  end,
}
