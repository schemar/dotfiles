-- Start terminal in insert mode
vim.api.nvim_create_autocmd({ "TermOpen" }, {
  pattern = "*",
  command = "startinsert",
})

-- LSP renaming support for nvim-tree using Snacks
-- Works with vtsls, but did not work with ts_ls or typescript-tools.
local prev = { new_name = "", old_name = "" } -- Prevents duplicate events
vim.api.nvim_create_autocmd("User", {
  pattern = "NvimTreeSetup",
  callback = function()
    local events = require("nvim-tree.api").events
    events.subscribe(events.Event.NodeRenamed, function(data)
      if prev.new_name ~= data.new_name or prev.old_name ~= data.old_name then
        prev = data
        require("snacks").rename.on_rename_file(data.old_name, data.new_name)
      end
    end)
  end,
})
