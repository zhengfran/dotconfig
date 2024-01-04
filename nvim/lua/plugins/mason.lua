return {
	-- lsp servers , linters, and formatters
	"williamboman/mason.nvim",

	dependencies = {
		"williamboman/mason-lspconfig.nvim",
		"jayp0521/mason-null-ls.nvim", -- bridges gap b/w mason & null-ls
	},

	lazy = false,

	config = function()
		require("mason").setup()
		require("mason-lspconfig").setup({
			-- list of servers for mason to install
			ensure_installed = {
				"clangd",
				"lua_ls",
				"jsonls",
				"dockerls",
				"rust_analyzer",
				"pyright",
				"cmake",
			},
			-- auto-install configured servers (with lspconfig)
			automatic_installation = true, -- not the same as ensure_installed
		})

		require("mason-null-ls").setup({
			-- list of formatters & linters for mason to install
			ensure_installed = {
				"stylua", -- lua formatter
				"clang_format", -- c/cpp formatter
				"rustfmt", -- rust formatter
				"jq", -- json formatter
			},
			-- auto-install configured formatters & linters (with null-ls)
			automafic_installation = true,
		})
	end,
}
