return {
  "akinsho/toggleterm.nvim",
  event = { "VeryLazy" },
  version = "*",
  opts = {
    open_mapping = [[<C-e>]],
    insert_mappings = true,   -- whether or not the open mapping applies in insert mode
    terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
    direction = "horizontal",
    close_on_exit = false,
    start_in_insert = true,
    size = function(term)
      if term.direction == "horizontal" then
        return 30
      elseif term.direction == "vertical" then
        return vim.o.columns * 0.4
      end
    end
  },
}
