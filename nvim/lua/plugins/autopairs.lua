return {
	"windwp/nvim-autopairs", -- autoclose parens, brackets, quotes, etc...
	dependencies = {
		"hrsh7th/nvim-cmp",
	},
	lazy = false,
	config = function()
		require("nvim-autopairs").setup({
			check_ts = true, --enable treesitter
			ts_config = {
				lua = { "string" }, --don't add pairs in lua string treesitter node
				java = false, --don;t check treesitter on java
			},
		})
		local cmp_autopairs = require("nvim-autopairs.completion.cmp")
		local cmp = require("cmp")

		-- make autopairs and completion work together
		cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
	end,
}
