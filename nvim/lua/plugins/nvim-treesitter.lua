return {
	"nvim-treesitter/nvim-treesitter",
	build = function()
		local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
		ts_update()
	end,
	dependencies = {
		{ "windwp/nvim-ts-autotag" }, -- autoclose tags
	},
	config = function()
		require("nvim-treesitter.configs").setup({
			-- enable syntax highlighting
			highlight = {
				enable = true,
			},
			-- enable indentation
			indent = { enable = true },
			-- enable autotagging (w/ nvim-ts-autotag plugin)
			autotag = { enable = true },
			-- ensure these language parsers are installed
			ensure_installed = {
				"c",
				"cpp",
				"json",
				"yaml",
				"xml",
				"groovy",
				"markdown",
				"markdown_inline",
				"bash",
				"lua",
				"vim",
				"dockerfile",
				"gitignore",
			},
			-- auto install above language parsers
			auto_install = true,
		})
	end,
}
