return {
  "akinsho/bufferline.nvim",
  event = "VeryLazy",
  keys = {
    { "<leader>bp", "<Cmd>BufferLineTogglePin<CR>", desc = "Toggle Pin" },
    { "<leader>bP", "<Cmd>BufferLineGroupClose ungrouped<CR>", desc = "Delete Non-Pinned Buffers" },
    { "<leader>br", "<Cmd>BufferLineCloseRight<CR>", desc = "Delete Buffers to the Right" },
    { "<leader>bl", "<Cmd>BufferLineCloseLeft<CR>", desc = "Delete Buffers to the Left" },
    {
      "<leader>bt",
      "<Cmd>BufferLineGroupClose<CR>",
      desc = "toggle BufferLine Groups",
    },
    { "<S-h>", "<cmd>BufferLineCyclePrev<cr>", desc = "Prev Buffer" },
    { "<S-l>", "<cmd>BufferLineCycleNext<cr>", desc = "Next Buffer" },
    { "[b", "<cmd>BufferLineCyclePrev<cr>", desc = "Prev Buffer" },
    { "]b", "<cmd>BufferLineCycleNext<cr>", desc = "Next Buffer" },
    { "[B", "<cmd>BufferLineMovePrev<cr>", desc = "Move buffer prev" },
    { "]B", "<cmd>BufferLineMoveNext<cr>", desc = "Move buffer next" },
  },
  opts = {
    options = {
      -- stylua: ignore
      close_command = function(n) Snacks.bufdelete(n) end,
      -- stylua: ignore
      right_mouse_command = function(n) Snacks.bufdelete(n) end,
      diagnostics = "nvim_lsp",
      groups = {
        items = {
          {
            name = "Code",
            matcher = function(buf)
              return buf.name:match("%.c$")
                or buf.name:match("%.rs$")
                or buf.name:match("%.lua$")
                or buf.name:match("%.h$")
            end,
          },
          {
            name = "Docs",
            matcher = function(buf)
              return buf.name:match("%.md$") or buf.name:match("%.org$") or buf.name:match("%.txt$")
            end,
          },
          {
            name = "Debug",
            matcher = function(buf)
              return buf.name:match("%.t32$") or buf.name:match("%.cmm$")
            end,
          },
          {
            name = "Script",
            matcher = function(buf)
              return buf.name:match("%.sh$") or buf.name:match("%.bat$") or buf.name:match("%.ps1$")
            end,
          },
          {
            name = "Config",
            matcher = function(buf)
              return buf.name:match("%.json$") or buf.name:match("%.toml$") or buf.name:match("%.conf$")
            end,
          },
          {
            name = "Log",
            matcher = function(buf)
              return buf.name:match("%.txt$") or buf.name:match("%.log$")
            end,
          },
        },
      },
      always_show_bufferline = false,
      diagnostics_indicator = function(_, _, diag)
        local icons = LazyVim.config.icons.diagnostics
        local ret = (diag.error and icons.Error .. diag.error .. " " or "")
          .. (diag.warning and icons.Warn .. diag.warning or "")
        return vim.trim(ret)
      end,
      offsets = {
        {
          filetype = "neo-tree",
          text = "Neo-tree",
          highlight = "Directory",
          text_align = "left",
        },
        {
          filetype = "snacks_layout_box",
        },
      },
      ---@param opts bufferline.IconFetcherOpts
      get_element_icon = function(opts)
        return LazyVim.config.icons.ft[opts.filetype]
      end,
    },
  },
  config = function(_, opts)
    require("bufferline").setup(opts)
    -- Fix bufferline when restoring a session
    vim.api.nvim_create_autocmd({ "BufAdd", "BufDelete" }, {
      callback = function()
        vim.schedule(function()
          pcall(nvim_bufferline)
        end)
      end,
    })
  end,
}
