return {
  "ojroques/vim-oscyank",
  config = function()
    vim.keymap.set("v", "<leader>cy", "<Plug>OSCYankVisual")
  end,
}
