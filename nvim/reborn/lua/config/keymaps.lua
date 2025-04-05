vim.g.mapleader = " "
vim.g.maplocalleader = ","

local mode_nv = { "n", "v" }
local mode_v = { "v" }
local mode_i = { "i" }
local nmappings = {
  { from = "<c-s>", to = ":w<CR>", mode = mode_nv },
  { from = "<c-s>", to = "<Esc>:w<CR>", mode = mode_i },
  { from = "<c-s>", to = "<Esc>:w<CR>", mode = mode_v },
  { from = "<c-q>", to = ":q<CR>", mode = mode_nv },
  { from = "<c-q>", to = "<C-o>:q<CR>", mode = mode_i },
  { from = "<c-q>", to = "<Esc>:q<CR>", mode = mode_v },
  { from = "<c-y>", to = "<ESC>A {}<ESC>i<CR><ESC>ko", mode = mode_i },
  { from = "jj", to = "<ESC>", mode = mode_i },

  -- Other
  { from = "<leader>sw", to = ":set wrap<CR>" },
  { from = "<leader>sc", to = ":set spell!<CR>" },
  { from = "<leader>nh", to = ":nohlsearch<CR>" },
  { from = "<leader>o", to = "za" },
  { from = "<leader>rc", to = ":e ~/.config/nvim/init.lua<CR>" },
  { from = "<leader><esc>", to = "<nop>" },
}

vim.keymap.set("n", "q", "<nop>", { noremap = true })
vim.keymap.set("n", ",q", "q", { noremap = true })

for _, mapping in ipairs(nmappings) do
  vim.keymap.set(mapping.mode or "n", mapping.from, mapping.to, { noremap = true })
end

local function run_vim_shortcut(shortcut)
  local escaped_shortcut = vim.api.nvim_replace_termcodes(shortcut, true, false, true)
  vim.api.nvim_feedkeys(escaped_shortcut, "n", true)
end

-- close win below
vim.keymap.set("n", "<leader>q", function()
  require("trouble").close()
  local wins = vim.api.nvim_tabpage_list_wins(0)
  if #wins > 1 then
    run_vim_shortcut([[<C-w>j:q<CR>]])
  end
end, { noremap = true, silent = true })
