local setup, bufferline = pcall(require, "bufferline")
if not setup then
	return
end

bufferline.setup({
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
