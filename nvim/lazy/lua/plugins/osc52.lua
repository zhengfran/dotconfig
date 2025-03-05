return {
  "ojroques/vim-oscyank",
  config = function()
    vim.keymap.set("v", "<leader>oo", "<Plug>OSCYankVisual")
  end,
}
