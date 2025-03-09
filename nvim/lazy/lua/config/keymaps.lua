-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local mode_n = { "n" }
local mode_i = { "i" }
local mode_v = { "v" }
-- local mode_nv = { "n", "v" }
local nmappings = {
  { from = "<c-q>",      to = ":q<CR>",          mode = mode_n },
  { from = "<c-q>",      to = "<C-o>:q<CR>",     mode = mode_i },
  { from = "jj",         to = "<Esc>",           mode = mode_i },
  { from = "<c-q>",      to = "<Esc>:q<CR>",     mode = mode_v },
  { from = "<leader>qf", to = "<cmd>cclose<CR>", mode = mode_n },
}

for _, mapping in ipairs(nmappings) do
  vim.keymap.set(mapping.mode or "n", mapping.from, mapping.to, { noremap = true })
end
