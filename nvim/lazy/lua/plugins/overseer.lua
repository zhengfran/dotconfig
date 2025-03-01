return {
  "stevearc/overseer.nvim",
  cmd = { "OverseerRun", "OverseerToggle" }, -- Lazy-load on these commands
  keys = {
    { "<leader>rr", "<cmd>OverseerRun<CR>", desc = "Run Task" },
    { "<leader>rt", "<cmd>OverseerToggle<CR>", desc = "Toggle Overseer" },
  },
  config = function()
    local overseer = require("overseer")

    overseer.setup({
      templates = { "builtin" }, -- Keep built-in templates
      task_list = {
        default_detail = 1, -- Controls UI verbosity
        max_width = { 100, 0.4 }, -- Dynamic width
      },
      form = {
        border = "rounded",
        win_opts = { winblend = 10 },
      },
    })

    -- Register which-key mappings
    local wk = require("which-key")
    wk.add({
      { "<leader>r", group = "run" },
    })
    -- Task to build a single C/C++ file
    overseer.register_template({
      name = "Build Current C/C++ File",
      builder = function()
        local file = vim.fn.expand("%:p") -- Get full path of current file
        local output = vim.fn.expand("%:p:r") -- Remove extension for output binary
        local compiler = vim.bo.filetype == "cpp" and "g++" or "gcc"

        return {
          name = "Build: " .. vim.fn.expand("%:t"), -- Task name based on filename
          cmd = { compiler },
          args = { file, "-o", output, "-g", "-Wall", "-Wextra" },
          components = {
            { "on_output_quickfix", mode = "errors", open = true }, -- ✅ Use loclist instead of quickfix
            "default",
          },
          metadata = { output_file = output }, -- Store output file path
        }
      end,
      condition = {
        filetype = { "c", "cpp" },
      },
    })

    -- Task to run the compiled executable
    overseer.register_template({
      name = "Run Compiled C/C++ File",
      builder = function()
        local output = vim.fn.expand("%:p:r") -- Get the compiled executable path

        return {
          name = "Run: " .. vim.fn.expand("%:t:r"), -- Task name based on executable
          cmd = { output }, -- Fixed execution path
          components = {
            { "on_output_quickfix", mode = "errors", open = true }, -- ✅ Use loclist instead of quickfix
            "default",
          },
        }
      end,
      condition = {
        filetype = { "c", "cpp" },
      },
    })
  end,
}
