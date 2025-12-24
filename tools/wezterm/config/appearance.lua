local wezterm = require("wezterm")
local gpu_adapters = require("utils.gpu_adapter")
local colors = require("colors.custom")

-- Try to pick the best GPU adapter, fallback to nil if not found
local preferred_gpu = gpu_adapters:pick_best()
return {
	animation_fps = 60,
	max_fps = 60,
        -- Fallback mechanism for the front_end
        front_end = preferred_gpu and "WebGpu" or "Software",
        
        -- WebGPU settings if a preferred GPU is found
        webgpu_power_preference = "HighPerformance",
        webgpu_preferred_adapter = preferred_gpu,

	-- color scheme
	colors = colors,

	-- background
	background = {
		{
			source = { File = wezterm.GLOBAL.background },
			horizontal_align = "Center",
		},
		{
			source = { Color = colors.background },
			height = "100%",
			width = "100%",
			opacity = 0.96,
		},
	},

	-- scrollbar
	enable_scroll_bar = true,

	-- tab bar
	enable_tab_bar = true,
	hide_tab_bar_if_only_one_tab = false,
	use_fancy_tab_bar = false,
	tab_max_width = 25,
	show_tab_index_in_tab_bar = false,
	switch_to_last_active_tab_when_closing_tab = true,

	-- window
	window_padding = {
		left = 5,
		right = 10,
		top = 12,
		bottom = 7,
	},
	window_close_confirmation = "NeverPrompt",
	window_frame = {
		active_titlebar_bg = "#090909",
		-- font = fonts.font,
		-- font_size = fonts.font_size,
	},
	inactive_pane_hsb = {
		saturation = 0.7,
		brightness = 0.65,
	},
}
