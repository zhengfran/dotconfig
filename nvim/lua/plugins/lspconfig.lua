return {
	"neovim/nvim-lspconfig",

	dependencies = {
		"williamboman/mason.nvim",
		"williamboman/mason-lspconfig.nvim",
		"glepnir/lspsaga.nvim",
		"hrsh7th/cmp-nvim-lsp",
	},
	event = { "BufReadPost", "BufNewFile" },
	cmd = { "LspInfo", "LspInstall", "LspUninstall" },

	config = function()
		local keymap = vim.keymap -- for conciseness

		-- Command to toggle inline diagnostics
		vim.api.nvim_create_user_command("DiagnosticsToggleVirtualText", function()
			local current_value = vim.diagnostic.config().virtual_text
			if current_value then
				vim.diagnostic.config({ virtual_text = false })
			else
				vim.diagnostic.config({ virtual_text = true })
			end
		end, {})

		-- Command to toggle diagnostics
		vim.api.nvim_create_user_command("DiagnosticsToggle", function()
			local current_value = vim.diagnostic.is_disabled()
			if current_value then
				vim.diagnostic.enable()
			else
				vim.diagnostic.disable()
			end
		end, {})
		-- enable keybinds only for when lsp server available
		local on_attach = function(client, bufnr)
			-- keybind options
			local opts = { noremap = true, silent = true, buffer = bufnr }

			-- set keybinds
			keymap.set("n", "gf", "<cmd>Lspsaga lsp_finder<CR>", opts) -- show definition, references
			keymap.set("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts) -- got to declaration
			keymap.set("n", "gd", "<cmd>Lspsaga peek_definition<CR>", opts) -- see definition and make edits in window
			keymap.set("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts) -- go to implementation
			keymap.set("n", "<leader>ca", "<cmd>Lspsaga code_action<CR>", opts) -- see available code actions
			keymap.set("n", "<leader>rn", "<cmd>Lspsaga rename<CR>", opts) -- smart rename
			keymap.set("n", "<leader>D", "<cmd>Lspsaga show_line_diagnostics<CR>", opts) -- show  diagnostics for line
			keymap.set("n", "<leader>d", "<cmd>Lspsaga show_cursor_diagnostics<CR>", opts) -- show diagnostics for cursor
			keymap.set("n", "[d", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts) -- jump to previous diagnostic in buffer
			keymap.set("n", "]d", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts) -- jump to next diagnostic in buffer
			keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>", opts) -- show documentation for what is under cursor
			keymap.set("n", "<leader>ol", "<cmd>Lspsaga outline<CR>", opts) -- see outline on right hand side
			keymap.set("n", "<leader>dt", ":DiagnosticsToggle<CR>", opts) -- see outline on right hand side
		end

		-- used to enable autocompletion (assign to every lsp server config)
		local capabilities = require("cmp_nvim_lsp").default_capabilities()

		-- Change the Diagnostic symbols in the sign column (gutter)
		-- (not in youtube nvim video)
		local signs = { Error = " ", Warn = " ", Hint = "ﴞ ", Info = " " }
		for type, icon in pairs(signs) do
			local hl = "DiagnosticSign" .. type
			vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
		end
		local lspconfig = require("lspconfig")
		lspconfig["clangd"].setup({
			capabilities = capabilities,
			on_attach = on_attach,
			cmd = {
				"clangd",
				"--offset-encoding=utf-16",
			},
		})
		lspconfig["rust_analyzer"].setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig["jsonls"].setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig["dockerls"].setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig["cmake"].setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
		lspconfig["lua_ls"].setup({
			capabilities = capabilities,
			on_attach = on_attach,
			settings = { -- custom settings for lua
				Lua = {
					-- make the language server recognize "vim" global
					diagnostics = {
						globals = { "vim" },
					},
					workspace = {
						-- make language server aware of runtime files
						library = {
							[vim.fn.expand("$VIMRUNTIME/lua")] = true,
							[vim.fn.stdpath("config") .. "/lua"] = true,
						},
					},
				},
			},
		})
		lspconfig["pyright"].setup({
			capabilities = capabilities,
			on_attach = on_attach,
		})
	end,
}
