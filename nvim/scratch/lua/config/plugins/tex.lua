return {
	"lervag/vimtex",
	ft = 'tex',
	init = function()
		vim.g.vimtex_view_method = "zathura"
		vim.g.vimtex_quickfix_ignore_filters = {
			'Font shape',
			"badness 10000",
			"Package hyperref Warning",
		}
	end
}
