return {
	"nvim-lualine/lualine.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	lazy = false,

	config = function()
		lualine = require("lualine")
		lualine_nightfly = require("lualine.themes.nightfly")
		local new_colors = {
			blue = "#65D1FF",
			green = "#3EFFDC",
			violet = "#FF61EF",
			yellow = "#FFDA7B",
			black = "#000000",
		}
		-- change nightlfy theme colors
		lualine_nightfly.normal.a.bg = new_colors.blue
		lualine_nightfly.insert.a.bg = new_colors.green
		lualine_nightfly.visual.a.bg = new_colors.violet
		lualine_nightfly.command = {
			a = {
				gui = "bold",
				bg = new_colors.yellow,
				fg = new_colors.black, -- black
			},
		}

		-- configure lualine with modified theme
		lualine.setup({
			options = {
				theme = lualine_nightfly,
			},
			sections = {
				lualine_x = {
					{
						require("lazy.status").updates,
						cond = require("lazy.status").has_updates,
						color = { fg = "#ff9e64" },
					},
				},
			},
		})
	end,
}
