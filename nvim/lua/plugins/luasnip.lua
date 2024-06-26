return {
  {
    "L3MON4D3/LuaSnip",
    config = function()
      require("luasnip").config.set_config {
        history = true,
        enable_autosnippets = true,
        store_selection_keys = "`",
        ext_opts = {
          [require("luasnip.util.types").choiceNode] = {
            active = {
              virt_text = { { "●", "GruvboxOrange" } },
            },
          },
        },
      }
      require("luasnip.loaders.from_lua").load { paths = "~/dotconfig/nvim/lua/LuaSnip" }
      local auto_expand = require("luasnip").expand_auto
      require("luasnip").expand_auto = function(...)
        vim.o.undolevels = vim.o.undolevels
        auto_expand(...)
      end
      -- local types = require("luasnip.util.types")
      -- require("luasnip").config.setup({
      -- ext_opts = {
      --   [types.choiceNode] = {
      --    active = {
      --       virt_text = { { "●", "GruvboxOrange" } },
      --     },
      -- },
      --     [types.insertNode] = {
      --       active = {
      --         virt_text = { { "●", "GruvboxBlue" } },
      --       },
      --     },
      --   },
      -- })
    end,
    keys = function()
      return {
        {
          "fj",
          function() return require("luasnip").expand_or_locally_jumpable() and "<Plug>luasnip-jump-next" end,
          expr = true,
          silent = true,
          mode = "i",
        },
        {
          "fj",
          function() return require("luasnip").jump(1) end,
          mode = "s",
        },
        {
          "fk",
          function() require("luasnip").jump(-1) end,
          mode = { "i", "s" },
        },
        {
          "<c-j>",
          "<Plug>luasnip-next-choice",
          mode = { "i", "s" },
        },
        {
          "<c-k>",
          "<Plug>luasnip-prev-choice",
          mode = { "i", "s" },
        },
        -- {
        --   "<tab>",
        --   function()
        --     if require("luasnip").expand_or_jumpable() then
        --       require("luasnip").expand_or_jump()
        --     else
        --       return "<tab>"
        --     end
        --   end,
        --   mode = { "i", "s" },
        -- },
      }
    end,
  },
}
