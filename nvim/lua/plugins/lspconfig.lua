return {
	"neovim/nvim-lspconfig",

	dependencies = {
		"williamboman/mason.nvim",
		"williamboman/mason-lspconfig.nvim",
		"glepnir/lspsaga.nvim",
	},
	event = { "BufReadPost", "BufNewFile" },
	cmd = { "LspInfo", "LspInstall", "LspUninstall" },

	--config = function()
	--    require("plugins.lsp.nvim-lspconfig")
	--end,
}
