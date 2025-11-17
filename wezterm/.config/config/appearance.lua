return function(wezterm, config)
	-- 默认的长宽
	config.initial_cols = 120
	config.initial_rows = 28

	config.window_decorations = "RESIZE"
	-- 关掉才可以有顶部栏的样式调整
	config.use_fancy_tab_bar = false
	config.tab_max_width = 25
	config.hide_tab_bar_if_only_one_tab = false
  config.color_scheme = 'Catppuccin Macchiato'	
	config.window_background_opacity = 0.9
	config.font = wezterm.font('Iosevka', { weight = 'Medium', italic = false })
	if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
		config.default_prog = { 'powershell.exe' }
		config.font_size = 14
	elseif wezterm.target_triple == 'aarch64-apple-darwin' then
		config.default_prog = {'/usr/bin/zsh'}
	else -- Linux
		config.default_prog = {'/usr/bin/bash'}
	end
		
	-- 标签栏与窗口边缘的空隙
	config.window_padding = {
		top = 0,
	}
end
