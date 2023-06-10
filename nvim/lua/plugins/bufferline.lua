return {
	"akinsho/bufferline.nvim",
	version = "*",
	dependencies = {
		"nvim-tree/nvim-web-devicons",
	},
	event = "BufEnter",

	keys = {
		{ "<M-h>", ":BufferLineCyclePrev<CR>" },
		{ "<M-l>", ":BufferLineCycleNext<CR>" },
		{ "<M-1>", ":BufferLineGoToBuffer 1<CR>" },
		{ "<M-2>", ":BufferLineGoToBuffer 2<CR>" },
		{ "<M-3>", ":BufferLineGoToBuffer 3<CR>" },
		{ "<M-4>", ":BufferLineGoToBuffer 4<CR>" },
		{ "<M-5>", ":BufferLineGoToBuffer 5<CR>" },
		{ "<M-6>", ":BufferLineGoToBuffer 6<CR>" },
		{ "<M-7>", ":BufferLineGoToBuffer 7<CR>" },
		{ "<M-8>", ":BufferLineGoToBuffer 8<CR>" },
		{ "<M-9>", ":BufferLineGoToBuffer 9<CR>" },
	},

	config = function()
		require("bufferline").setup({
			options = {
				-- 为每个 buffer 都配置一个序数
				numbers = "ordinal",
				-- 使用内置 LSP 进行诊断
				diagnostics = "nvim_lsp",
				separator_style = "thin",
				-- 左侧让出 nvim-tree 的位置
				offsets = {
					{
						filetype = "NvimTree",
						text = "File Explorer",
						highlight = "Directory",
						text_align = "left",
					},
				},
			},
		})
	end,
}
