return {
  {
    "nvimdev/lspsaga.nvim",
    event = "LspAttach",
    config = function()
      require("lspsaga").setup({
        ui = {
          border = "rounded",
        },
        definition = {
          win_config = {
            relative = "editor",
            anchor = "NW", -- intended position: top left corner
            row = 2,
            col = 2,
            width = 80,
            height = 20,
          },
        },
        finder = {
          win_config = {
            relative = "editor",
            anchor = "NW",
            row = 2,
            col = 2,
            width = 80,
            height = 20,
          },
        },
      })
    end,
    keys = {
      { "<leader>pd", "<cmd>Lspsaga peek_definition<CR>", desc = "Peek Definition" },
      { "<leader>pr", "<cmd>Lspsaga finder<CR>", desc = "Peek References" },
    },
  },
}
