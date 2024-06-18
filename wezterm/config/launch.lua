local platform = require("utils.platform")()

local options = {
	default_prog = {},
	launch_menu = {},
}

if platform.is_win then
	options.default_prog = { "powershell" }
	options.launch_menu = {
		{ label = "PowerShell Core", args = { "pwsh" } },
		{ label = "PowerShell Desktop", args = { "powershell" } },
		{ label = "Command Prompt", args = { "cmd" } },
		{
			label = "Git Bash",
			args = { "C:\\Users\\uie41442\\scoop\\apps\\git\\current\\bin\\bash.exe" },
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
