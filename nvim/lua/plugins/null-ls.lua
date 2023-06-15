return {
    "jose-elias-alvarez/null-ls.nvim", -- configure formatters & linters

    dependencies = {
	    "nvim-lua/plenary.nvim", --lua functions that many plugins use
    },

    lazy = false,
    keys = {
        {"<leader>fr", "<cmd>lua vim.lsp.buf.format()<CR>"},
    },

    config =  function()
        local null_ls = require("null-ls")
        local formatting = null_ls.builtins.formatting
        -- local diagnostics = null_ls.builtins.diagnostics

        -- to setup format on save
        -- local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

        null_ls.setup({
        	sources = {
        		formatting.stylua,
        		formatting.clang_format,
        	},
        	-- configure format on save
        	-- on_attach = function(current_client, bufnr)
        	-- 	-- (not in youtube nvim video)
        	-- 	if current_client.supports_method("textDocument/formatting") then
        	-- 		vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
        	-- 		vim.api.nvim_create_autocmd("BufWritePre", {
        	-- 			group = augroup,
        	-- 			buffer = bufnr,
        	-- 			callback = function()
        	-- 				vim.lsp.buf.format({
        	-- 					filter = function(client)
        	-- 						--  only use null-ls for formatting instead of lsp server
        	-- 						return client.name == "null-ls"
        	-- 					end,
        	-- 					bufnr = bufnr,
        	-- 				})
        	-- 			end,
        	-- 		})
        	-- 	end
        	-- end,
        })
    end

}
