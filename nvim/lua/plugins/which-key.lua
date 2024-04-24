return {
	"folke/which-key.nvim", -- which-key

	config = function()
		local wk = require("which-key")
		wk.setup()
	end,
}
