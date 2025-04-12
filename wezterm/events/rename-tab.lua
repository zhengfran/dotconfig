local wezterm = require('wezterm')

local M = {}

M.setup = function()
    wezterm.on("rename-tab", function(window, pane)
        window:perform_action(
          wezterm.action.PromptInputLine {
            description = "Enter new tab title:",
            action = wezterm.action_callback(function(_, _, line)
              if line then
                local tab = window:mux_window():active_tab()
                if tab then
                  tab:set_title_override(line)
                end
              end
            end)
          },
          pane
        )
      end)
end

return M