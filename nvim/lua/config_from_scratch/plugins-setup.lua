local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

require ('lazy').setup({

	"nvim-lua/plenary.nvim", --lua functions that many plugins use

	"bluz71/vim-nightfly-guicolors", --preferred colorscheme

	"christoomey/vim-tmux-navigator", --tmux & split window navigation

	"szw/vim-maximizer", --maximize and restores current window

	--make editing a lot easier
	"tpope/vim-surround",
	"vim-scripts/ReplaceWithRegister",

	"numToStr/Comment.nvim", -- commenting with gc

	"nvim-tree/nvim-tree.lua", --file explorer

	"nvim-tree/nvim-web-devicons", --icons

	"nvim-lualine/lualine.nvim", --statusline

	{ "akinsho/bufferline.nvim", version="*"},

	--fuzzy finding
	{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" }, --dependency
	{ "nvim-telescope/telescope.nvim", branch = "0.1.x" }, --dependency

	--- autocompletion
	"hrsh7th/nvim-cmp",
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",

	-- snippets
	"rafamadriz/friendly-snippets",

	-- lsp servers , linters, and formatters
	"williamboman/mason.nvim",
	"williamboman/mason-lspconfig.nvim",

	--configuring lsp servers
	"neovim/nvim-lspconfig",
	"hrsh7th/cmp-nvim-lsp", -- for autocompletion
	{ "glepnir/lspsaga.nvim", branch = "main" }, -- enhanced lsp use
	"onsails/lspkind.nvim", -- vs-code like icons for autocompletion

	-- formatting & linting
	"jose-elias-alvarez/null-ls.nvim", -- configure formatters & linters
	"jayp0521/mason-null-ls.nvim", -- bridges gap b/w mason & null-ls

	-- auto closing
	"windwp/nvim-autopairs", -- autoclose parens, brackets, quotes, etc...
	-- treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
			ts_update()
		end,
        dependencies = {
	        { "windwp/nvim-ts-autotag"}, -- autoclose tags
        }
	},

	"lewis6991/gitsigns.nvim", -- git signs

    "akinsho/toggleterm.nvim", -- toggleterm

	"Pocco81/auto-save.nvim", -- Autosave

    "folke/which-key.nvim", -- which-key

    "f-person/git-blame.nvim", -- git blame

    "ojroques/nvim-osc52", -- OSC52

})
