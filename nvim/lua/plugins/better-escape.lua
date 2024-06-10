return {
  "max397574/better-escape.nvim",
  -- enabled = true,
  config = function()
    require("better_escape").setup {
      mapping = { "fd", "jk" }, -- a table with mappings to use
      clear_empty_lines = false, -- clear line after escaping if there is only whitespace
      keys = "<Esc>",
    }
  end,
}
