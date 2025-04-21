-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local function mapkey(mode, lhs, rhs)
  vim.api.nvim_set_keymap(mode, lhs, rhs, { noremap = true })
end

local function mapcmd(key, cmd)
  vim.api.nvim_set_keymap("n", key, ":" .. cmd .. "<cr>", { noremap = true })
end

-- use arrow key for resize window
mapcmd("<leader><up>", "res +5")
mapcmd("<leader><down>", "res -5")
mapcmd("<leader><left>", "vertical resize-5")
mapcmd("<leader><right>", "vertical resize+5")
-- use leader q for closing window
mapkey("", "<leader>qj", "<C-w>j:q<CR>")
mapkey("", "<leader>qk", "<C-w>k:q<CR>")
mapkey("", "<leader>qh", "<C-w>h:q<CR>")
mapkey("", "<leader>ql", "<C-w>l:q<CR>")
-- use tab for indentation
mapkey("x", "<s-tab>", "<gv")
mapkey("x", "<tab>", ">gv")

function Search_and_replace_current_file()
  local search_text = vim.fn.input("Search for in current file: ")

  local replace_text = vim.fn.input("Replace with: ")

  if search_text ~= "" and replace_text ~= "" then
    local cmd = string.format("!sed -i 's/%s/%s/g' %%", search_text, replace_text)
    vim.cmd(cmd)
    print("Replaced all occurrences of '" .. search_text .. "' with '" .. replace_text .. "' in current file.")
  else
    print("Search or replace text cannot be empty.")
  end
end
mapcmd("<leader>sf", "lua Search_and_replace_current_file()")
