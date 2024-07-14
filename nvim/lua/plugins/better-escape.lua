return {
  "max397574/better-escape.nvim",
  enabled = true,
  config = function()
    require("better_escape").setup {
      timeout = vim.o.timeoutlen,
      default_mappings = false,
      mappings = {
        i = {
          f = {
            -- These can all also be functions
            d = "<Esc>",
          },
        },
        c = {
          f = {
            d = "<Esc>",
          },
        },
        t = {
          f = {
            d = "<Esc>",
          },
        },
        v = {
          f = {
            d = "<Esc>",
          },
        },
        s = {
          f = {
            d = "<Esc>",
          },
        },
      },
    }
  end,
}
