return {
	"tom-anders/telescope-vim-bookmarks.nvim",
	dependencies = {
		"MattesGroeger/vim-bookmarks",
	},
	config = function()
		-- vim.g.bookmark_save_per_working_dir = 1
		vim.g.bookmark_auto_save = 1
		vim.g.bookmark_highlight_lines = 1
	end,
}
