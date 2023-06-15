return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.2",
	dependencies = {
		"nvim-lua/plenary.nvim",
		{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		"nvim-telescope/telescope-project.nvim",
		"tom-anders/telescope-vim-bookmarks.nvim",
		"nvim-telescope/telescope-live-grep-args.nvim",
		-- "nvim-telescope/telescope-file-browser.nvim",
	},

	config = function()
		local telescope = require("telescope")
		local actions = require("telescope.actions")
		-- local project_actions = require("telescope._extensions.project.actions")
        local lga_actions = require("telescope-live-grep-args.actions")
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
					find_command = { "fd" },
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
				-- project = {
				-- 	base_dirs = {
				-- 		"~/dotconfig",
				-- 	},
				-- 	hidden_files = false, -- default: false
				-- 	theme = "dropdown",
				-- 	order_by = "asc",
				-- 	search_by = "title",
				-- 	sync_with_nvim_tree = true, -- default false
				-- 	-- default for on_project_selected = find project files
				-- 	-- on_project_selected = function(prompt_bufnr)
				-- 	-- 	-- Do anything you want in here. For example:
				-- 	-- 	project_actions.change_working_directory(prompt_bufnr, false)
				-- 	-- 	require("harpoon.ui").nav_file(1)
				-- },
				live_grep_args = {
					auto_quoting = true, -- enable/disable auto-quoting
					-- define mappings, e.g.
					mappings = { -- extend mappings
						i = {
							["<C-k>"] = lga_actions.quote_prompt(),
							["<C-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
						},
					},
				},
				-- file_browser = {
				-- 	theme = "ivy",
				-- 	-- disables netrw and use telescope-file-browser in its place
				-- 	hijack_netrw = true,
				-- },
			},
		})
		telescope.load_extension("fzf")
		telescope.load_extension("vim_bookmarks")
		telescope.load_extension("project")
		telescope.load_extension("live_grep_args")
		-- telescope.load_extension("file_browser")
		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader>ff", builtin.find_files, {})
		vim.keymap.set("n", "<leader>fg", builtin.live_grep, {})
		vim.keymap.set("n", "<leader>fz", ":lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>" )
		vim.keymap.set("n", "<leader>fw", builtin.grep_string, {})
		vim.keymap.set("n", "<leader>fb", builtin.buffers, {})
		vim.keymap.set("n", "<leader>fo", builtin.oldfiles, {})
		vim.keymap.set("n", "<leader>fh", builtin.help_tags, {})
		-- vim.keymap.set("n", "<leader>.", ":Telescope file_browser<CR>", {})
		vim.keymap.set("n", "<leader>fm", ":Telescope vim_bookmarks all<CR>", {})
		vim.keymap.set("n", "<C-p>", ":lua require'telescope'.extensions.project.project{}<CR>", {})
	end,
}
