local M = {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    animate = { enabled = false },
    bigfile = { enabled = false },
    dim = { enabled = false },
    explorer = { enabled = false },
    gitbrowse = { enabled = false },
    image = { enabled = false },
    indent = {
      enabled = true,
      animate = {
        enabled = false,
      },
    },
    input = { enabled = false },
    lazygit = { enabled = false },
    notifier = { enabled = true },
    picker = { enabled = false },
    profiler = { enabled = false },
    quickfile = { enabled = false },
    scope = { enabled = false },
    scratch = { enabled = false },
    scroll = { enabled = false },
    statuscolumn = {
      enabled = true,
      left = { "mark", "sign" }, -- priority of signs on the left (high to low)
      right = { "git" }, -- priority of signs on the right (high to low)
    },
    styles = { enabled = false },
    terminal = { enabled = false },
    toggle = { enabled = false },
    words = { enabled = true },
    zen = { enabled = false },
  },
}

local dashboard = {
  enabled = true,
  width = 80,
  preset = {
    header = "Don't be lazy; don't be shit ",
    keys = require("schemar.config.keymaps").dashboard(),
  },
  sections = {
    { section = "header" },
    {
      section = "keys",
      gap = 0,
      padding = 1,
      indent = 0,
      icon = "",
    },
    {
      section = "recent_files",
      gap = 0,
      padding = 1,
      indent = 1,
      icon = "",
      title = "Recent Files",
      cwd = true,
    },
    -- {
    --   section = "terminal",
    --   enabled = true,
    --   gap = 0,
    --   padding = 1,
    --   indent = 0,
    --   icon = "",
    --   title = "Notifications",
    --   key = "n",
    --   action = function()
    --     vim.ui.open("https://github.com/notifications")
    --   end,
    --   height = 7,
    --   cmd = "gh api /notifications --method GET --field per_page=5 --field page=1 | jq -r '.[] | \"\\(.subject?.title) (\\(.reason))\"'",
    -- },
    { section = "startup", padding = 1 },
  },
}

M.opts.dashboard = dashboard

-- Fidget replacement for LSP status report (as Snacks notification):
---@type table<number, {token:lsp.ProgressToken, msg:string, done:boolean}[]>
local progress = vim.defaulttable()
vim.api.nvim_create_autocmd("LspProgress", {
  ---@param ev {data: {client_id: integer, params: lsp.ProgressParams}}
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    local value = ev.data.params.value --[[@as {percentage?: number, title?: string, message?: string, kind: "begin" | "report" | "end"}]]
    if not client or type(value) ~= "table" then
      return
    end
    local p = progress[client.id]

    for i = 1, #p + 1 do
      if i == #p + 1 or p[i].token == ev.data.params.token then
        p[i] = {
          token = ev.data.params.token,
          msg = ("[%3d%%] %s%s"):format(
            value.kind == "end" and 100 or value.percentage or 100,
            value.title or "",
            value.message and (" **%s**"):format(value.message) or ""
          ),
          done = value.kind == "end",
        }
        break
      end
    end

    local msg = {} ---@type string[]
    progress[client.id] = vim.tbl_filter(function(v)
      return table.insert(msg, v.msg) or not v.done
    end, p)

    local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
    vim.notify(table.concat(msg, "\n"), "info", {
      id = "lsp_progress",
      title = client.name,
      opts = function(notif)
        notif.icon = #progress[client.id] == 0 and " "
          or spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
      end,
    })
  end,
})

return M
