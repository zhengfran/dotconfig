return {
	"nvim-tree/nvim-tree.lua", --file explorer
	lazy = false,

	config = function()
		--recommended settings from nvim-tree documentation
		vim.g.loaded = 1
		vim.g.loaded_netrwPlugin = 1
		-- add trailing / to directories
		vim.g.n_tree_add_trailing = 1

		-- change color for arrows in tree to light blue
		vim.cmd([[ highlight NvimTreeIndentMarker guifg=#3FC5FF ]])

		-- configure nvim-tree
		require("nvim-tree").setup({
			view = {
				adaptive_size = true,
				-- auto_resize = true
				side = "left",
				width = 30,
				relativenumber = false,
				preserve_window_proportions = false,
			},
			diagnostics = {
				enable = true,
				icons = {
					hint = "",
					info = "",
					warning = "",
					error = "",
				},
			},
			git = {
				-- enable git
				enable = true,
				ignore = false,
				timeout = 500,
			},
			-- change folder arrow icons
			renderer = {
				full_name = true,
				group_empty = true,
				special_files = {},
				symlink_destination = false,
				indent_markers = {
					enable = true,
				},
				icons = {
					git_placement = "signcolumn",
					show = {
						file = true,
						folder = true,
						folder_arrow = true,
						git = true,
					},
				},
			},
			-- disable window_picker for
			-- explorer to work well with
			-- window splits
			actions = {
				open_file = {
					resize_window = true,
					window_picker = {
						enable = false,
					},
				},
			},
		})
	end,

	keys = {
		--nvim-tree
		{ "<leader>op", ":NvimTreeToggle<CR>" },
		{ "<leader>of", ":NvimTreeFindFile<CR>" },
	},
}
