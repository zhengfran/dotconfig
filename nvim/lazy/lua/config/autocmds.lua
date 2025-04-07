-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- Disable autoformat for c files
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "c" },
  callback = function()
    vim.b.autoformat = false
  end,
})

local gpgGroup = vim.api.nvim_create_augroup("customGpg", { clear = true })

vim.api.nvim_create_autocmd({ "BufReadPre", "FileReadPre" }, {
  pattern = "*.gpg",
  group = gpgGroup,
  callback = function()
    -- Make sure nothing is written to shada file while editing an encrypted file.
    vim.opt_local.shada = nil
    -- We don't want a swap file, as it writes unencrypted data to disk
    vim.opt_local.swapfile = false
    -- Switch to binary mode to read the encrypted file
    vim.opt_local.bin = true
    -- Save the current 'ch' value to a buffer-local variable
    vim.b.ch_save = vim.opt_local.ch:get()
    vim.cmd("set ch=2")
  end,
})

vim.api.nvim_create_autocmd({ "BufReadPost", "FileReadPost" }, {
  pattern = "*.gpg",
  group = gpgGroup,
  callback = function()
    vim.cmd("'[,']!gpg -d 2> /dev/null")
    -- Switch to normal mode for editing
    vim.opt_local.bin = false
    -- Restore the 'ch' value from the buffer-local variable
    vim.opt_local.ch = vim.b.ch_save
    vim.cmd("unlet b:ch_save")
    vim.cmd(":doautocmd BufReadPost " .. vim.fn.expand("%:r"))
  end,
})

-- Convert all text to encrypted text before writing
vim.api.nvim_create_autocmd({ "BufWritePre", "FileWritePre" }, {
  pattern = "*.gpg",
  group = gpgGroup,
  callback = function()
    -- Switch to binary mode to write the encrypted file
    vim.opt_local.bin = true
    -- So we can avoid the armor option
    vim.cmd("'[,']!gpg -e 2>/dev/null")
  end,
})
-- Undo the encryption so we are back in the normal text, directly after the file has been written.
vim.api.nvim_create_autocmd({ "BufWritePost", "FileWritePost" }, {
  pattern = "*.gpg",
  group = gpgGroup,
  callback = function()
    vim.cmd("u")
    -- Switch to normal mode for editing
    vim.opt_local.bin = false
  end,
})
