-- ===
-- === Editor behavior
-- ===

vim.o.number = true
vim.opt.conceallevel = 2
vim.o.relativenumber = true
vim.o.cursorline = true
vim.o.wrap = true
vim.o.showcmd = true
vim.o.wildmenu = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.softtabstop = 2
vim.o.autochdir = true
vim.o.scrolloff = 4
vim.o.smartindent = true
vim.o.updatetime = 100
vim.o.mouse = "a"
vim.o.termguicolors = true
vim.opt.updatetime = 200
vim.opt.iskeyword = "_,49-57,A-Z,a-z"
vim.o.fileencodings = "utf-8,gb2312,gb18030,gbk,ucs-bom,cp936,latin1"
vim.o.enc = "utf8"
vim.o.swapfile = true
vim.o.undofile = true

-- enter last opened postion when reopen
vim.cmd([[au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif]])

-- highligh when yank
vim.cmd([[au TextYankPost * silent! lua vim.highlight.on_yank()]])
