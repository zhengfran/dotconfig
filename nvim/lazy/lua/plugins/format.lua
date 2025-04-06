return {
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        fish = { "fish_indent" },
        json = { "prettier" },
        sh = { "shfmt" },
        html = { "prettier" },
        rust = { "rustfmt" },
      },
    },
  },
}
