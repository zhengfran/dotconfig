return {
	"nvim-lualine/lualine.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	lazy = false,

	config = function()
		local lualine = require("lualine")

		-- configure lualine with modified theme
		lualine.setup({
			options = {
				theme = "tokyonight",
			},
			sections = {
				lualine_x = {
					{
						require("lazy.status").updates,
						cond = require("lazy.status").has_updates,
						color = { fg = "#ff9e64" },
					},
				},
				lualine_a = {
					{
						"filename",
						file_status = true, -- displays file status (readonly status, modified status)
						path = 2, -- 0 = just filename, 1 = relative path, 2 = absolute path
					},
				},
			},
		})
	end,
}
