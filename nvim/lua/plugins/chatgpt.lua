-- Lazy
local home = vim.fn.expand("$HOME")
return {
	"jackMort/ChatGPT.nvim",
	event = "VeryLazy",
	dependencies = {
		"MunifTanjim/nui.nvim",
		"nvim-lua/plenary.nvim",
		"folke/trouble.nvim",
		"nvim-telescope/telescope.nvim",
	},
	config = function()
		require("chatgpt").setup({
			api_key_cmd = "gpg --decrypt " .. home .. "/gpt_key.gpg",
		})

		-- key maps
		vim.keymap.set("n", "<leader>cg", ":ChatGPT<CR>", {})
		vim.keymap.set("n", "<leader>ca", ":ChatGPTActAs<CR>", {})
		vim.keymap.set("n", "<leader>cr", ":ChatGPTRun ", {})
	end,
}
