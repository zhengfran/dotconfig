-- bootstrappings
local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
		vim.cmd([[packadd packer.nvim]])
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

-- Autocommand that reloads neovim whenever you save this file
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins-setup.lua source <afile> | PackerCompile
  augroup end
]])

local status, packer = pcall(require, "packer")
if not status then
	return
end

return packer.startup(function(use)
	use("wbthomason/packer.nvim") --packer can manager itself

	use("nvim-lua/plenary.nvim") --lua functions that many plugins use

	use("bluz71/vim-nightfly-guicolors") --preferred colorscheme

	use("christoomey/vim-tmux-navigator") --tmux & split window navigation

	use("szw/vim-maximizer") --maximize and restores current window

	--make editing a lot easier
	use("tpope/vim-surround")
	use("vim-scripts/ReplaceWithRegister")

	use("numToStr/Comment.nvim") -- commenting with gc

	use("nvim-tree/nvim-tree.lua") --file explorer

	use("kyazdani42/nvim-web-devicons") --icons

	use("nvim-lualine/lualine.nvim") --statusline

	use({ "akinsho/bufferline.nvim", tag = "v3.*" })

	--fuzzy finding
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" }) --dependency
	use({ "nvim-telescope/telescope.nvim", branch = "0.1.x" }) --dependency

	--- autocompletion
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-buffer")
	use("hrsh7th/cmp-path")

	-- snippets
	use("L3MON4D3/LuaSnip")
	use("saadparwaiz1/cmp_luasnip")
	use("rafamadriz/friendly-snippets")

	-- lsp servers , linters, and formatters
	use("williamboman/mason.nvim")
	use("williamboman/mason-lspconfig.nvim")

	--configuring lsp servers
	use("neovim/nvim-lspconfig")
	use("hrsh7th/cmp-nvim-lsp") -- for autocompletion
	use({ "glepnir/lspsaga.nvim", branch = "main" }) -- enhanced lsp uis
	use("onsails/lspkind.nvim") -- vs-code like icons for autocompletion

	-- formatting & linting
	use("jose-elias-alvarez/null-ls.nvim") -- configure formatters & linters
	use("jayp0521/mason-null-ls.nvim") -- bridges gap b/w mason & null-ls

	-- auto closing
	use("windwp/nvim-autopairs") -- autoclose parens, brackets, quotes, etc...
	use({ "windwp/nvim-ts-autotag", after = "nvim-treesitter" }) -- autoclose tags
	-- treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = function()
			local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
			ts_update()
		end,
	})

	use("lewis6991/gitsigns.nvim") -- git signs

    use("akinsho/toggleterm.nvim") -- toggleterm

	use("Pocco81/auto-save.nvim") -- Autosave

    use("folke/which-key.nvim") -- which-key

	if packer_bootstrap then
		require("packer").sync()
	end
end)
