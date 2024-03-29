return {
	"ThePrimeagen/harpoon", --harpoon

	lazy = true, -- it lazyload itself
	config = function()
		local harpoon = require("harpoon")
		harpoon.setup({
			menu = {
				width = vim.api.nvim_win_get_width(0) - 4,
			},
		})
		vim.keymap.set("n", "<leader>hm", ":lua require('harpoon.mark').add_file()<CR>") -- mark file in harpoon
		vim.keymap.set("n", "<leader>hq", ":lua require('harpoon.ui').toggle_quick_menu()<CR>") -- mark file in harpoon
	end,
}
