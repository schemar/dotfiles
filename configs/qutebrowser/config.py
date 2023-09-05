import catppuccin

# load config configured via GUI:
config.load_autoconfig()

c.window.hide_decoration = True
c.qt.highdpi = True
c.fonts.default_size = "14pt"

# set the flavor you'd like to use
# valid options are 'mocha', 'macchiato', 'frappe', and 'latte'
# last argument (optional, default is False): enable the plain look for the menu rows
catppuccin.setup(c, 'latte', True)
