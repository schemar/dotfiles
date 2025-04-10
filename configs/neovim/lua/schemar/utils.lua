local M = {}

---Shallow deduplication of the keys in the given table.
---@param input_table table The table to deduplicate.
---@return table result A new table with unique keys.
function M.dedup(input_table)
  local seen = {}
  local result = {}

  for _, value in ipairs(input_table) do
    if not seen[value] then
      seen[value] = true
      table.insert(result, value)
    end
  end

  return result
end

---Check if the given buffer is a file buffer.
---Is not 100% reliable, but should work for most cases.
---@param bufnr number The number of the buffer to check.
---@return boolean is_file_buffer True if the buffer is a file buffer, false otherwise.
function M.is_file_buffer(bufnr)
  -- A file buffer typically has a name (file) and no type (like a special buffer)
  local buftype = vim.api.nvim_get_option_value("buftype", { buf = bufnr })
  local bufname = vim.api.nvim_buf_get_name(bufnr)

  return buftype == "" and bufname ~= ""
end

---Opens a terminal in a vertical split and runs the given command.
---@param command? string The command to run in the terminal.
---@return nil
function M.split_term(command)
  command = command or ""
  vim.cmd("split | terminal " .. command)
end

---Opens a terminal in a vertical split and runs the given command.
---If the previous buffer was a file buffer, it will be reloaded after the terminal closes.
---@param command? string The command to run in the terminal.
---@return nil
function M.split_term_and_edit(command)
  local prev_bufnr = vim.api.nvim_get_current_buf()

  -- Open a terminal and run the command
  M.split_term(command)
  local term_bufnr = vim.api.nvim_get_current_buf()

  -- React when the terminal command finishes
  vim.api.nvim_create_autocmd("TermClose", {
    buffer = term_bufnr,
    once = true,
    callback = function()
      -- Force close the current (terminal) window
      vim.api.nvim_win_close(0, true)
      -- Force close the terminal buffer
      vim.api.nvim_buf_delete(term_bufnr, { force = true })
      -- Go back to the previous buffer
      vim.api.nvim_set_current_buf(prev_bufnr)
      -- Call `edit` to re-load file content
      if M.is_file_buffer(prev_bufnr) then
        vim.cmd("edit")
      end
    end,
  })
end

return M
