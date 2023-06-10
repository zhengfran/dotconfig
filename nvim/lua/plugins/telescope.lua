return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.2",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope-fzf-native.nvim",
        "tom-anders/telescope-vim-bookmarks.nvim",
	},

	config = function()
		local telescope = require("telescope")
		local actions = require("telescope.actions")
		telescope.load_extension("fzf")
        telescope.load_extension('vim_bookmarks')
		telescope.setup({
			defaults = {
				mappings = {
					i = {
						["C-k>"] = actions.move_selection_previous,
						["C-j>"] = actions.move_selection_next,
						["C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
					},
				},
			},
            pickers = {
                find_files = {
                    theme = "dropdown",
                    previewer = false,
                    find_command = {"fd"},
                },
            },
			extensions = {
				fzf = {
					fuzzy = true, -- false will only do exact matching
					override_generic_sorter = true, -- override the generic sorter
					override_file_sorter = true, -- override the file sorter
					case_mode = "smart_case", -- or "ignore_case" or "respect_case"
					-- the default case_mode is "smart_case"
				},
			},
		})
		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader>ff", builtin.find_files, {})
		vim.keymap.set("n", "<leader>fg", builtin.live_grep, {})
		vim.keymap.set("n", "<leader>fb", builtin.buffers, {})
		vim.keymap.set("n", "<leader>fo", builtin.oldfiles, {})
		vim.keymap.set("n", "<leader>fh", builtin.help_tags, {})
		vim.keymap.set("n", "<leader>fm", ":Telescope vim_bookmarks all<CR>", {})
	end,
}
