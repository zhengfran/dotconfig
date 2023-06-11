return {
	"akinsho/toggleterm.nvim",
	version = "*",
	config = function()
		local toggleterm = require("toggleterm")
		toggleterm.setup({
			hide_numbers = true,
			shade_filetypes = {},
			shade_terminals = true,
			shading_factor = 2,
			start_in_insert = true,
			insert_mappings = true,
			persist_size = true,
			direction = "float",
			close_on_exit = true,
			shell = vim.o.shell,
			float_opts = {
				border = "double", -- 'single' | 'double' | 'shadow' | 'curved' | ... other options supported by win open
				winblend = 0,
				highlights = {
					border = "Normal",
					background = "Normal",
				},
			},
		})

		local Terminal = require("toggleterm.terminal").Terminal
		-- local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })
		local lazygit = Terminal:new({
			cmd = "lazygit",
			dir = "git_dir",
			direction = "float",
			float_opts = {
				border = "curved", -- 'single' | 'double' | 'shadow' | 'curved' | ... other options supported by win open
			},
			-- function to run on opening the terminal
			on_open = function(term)
				vim.cmd("startinsert!")
				vim.api.nvim_buf_set_keymap(term.bufnr, "n", "q", "<cmd>close<CR>", { noremap = true, silent = true })
			end,
			-- function to run on closing the terminal
			-- on_close = function(term)
			--   vim.cmd("Closing terminal")
			-- end,
		})

		function _LAZYGIT_TOGGLE()
			lazygit:toggle()
		end
	end,
    
    keys = { 
        {"<leader>tt", "<cmd>exe v:count1 . 'ToggleTerm'<CR>"}, --open toggle term 
        {"<leader>tg", "<cmd>lua _LAZYGIT_TOGGLE()<CR>"}, --open lazy git
    },
}
