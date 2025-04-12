local platform = require("utils.platform")()

local options = {
  default_prog = {},
  launch_menu = {},
}

if platform.is_win then
  options.default_prog = { "nu" }
  options.launch_menu = {
    { label = "PowerShell Core", args = { "pwsh" } },
    { label = "PowerShell Desktop", args = { "powershell" } },
    { label = "Command Prompt", args = { "cmd" } },
    {
      label = "Git Bash",
      args = { "C:\\Users\\uie41442\\scoop\\apps\\git\\current\\bin\\bash.exe" },
    },
    {
      label = "MSYS2 UCRT64",
      args = { "C:\\Users\\uie41442\\scoop\\apps\\msys2\\current\\usr\\bin\\bash.exe", "-l" },
      set_environment_variables = {
        MSYSTEM = "UCRT64",
        CHERE_INVOKING = "1",
      },
    },
  }
elseif platform.is_mac then
  options.default_prog = { "zsh", "-l" }
  options.launch_menu = {
    { label = "Bash", args = { "bash", "-l" } },
    { label = "Fish", args = { "/opt/homebrew/bin/fish", "-l" } },
    { label = "Zsh", args = { "zsh", "-l" } },
  }
elseif platform.is_linux then
  options.default_prog = { "zsh", "-l" }
  options.launch_menu = {
    { label = "Bash", args = { "bash", "-l" } },
    { label = "Zsh", args = { "zsh", "-l" } },
  }
end

return options
