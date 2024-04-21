return {
	"junegunn/vim-easy-align", -- autoclose parens, brackets, quotes, etc...
	lazy = false,
	config = function()
		vim.keymap.set("x", "ga", "<Plug>(EasyAlign)", { desc = "EasyAlign" })
		vim.keymap.set("n", "ga", "<Plug>(EasyAlign)", { desc = "EasyAlign" })
	end,
}
