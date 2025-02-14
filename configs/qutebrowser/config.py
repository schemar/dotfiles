import catppuccin

config.load_autoconfig()

# set the flavor you'd like to use
# valid options are 'mocha', 'macchiato', 'frappe', and 'latte'
# last argument (optional, default is False): enable the plain look for the
# menu rows
catppuccin.setup(c, 'macchiato', False)

# Always restore open sites when qutebrowser is reopened. Without this option
# set, :wq (:quit --save) needs to be used to save open tabs (and restore
# them), while quitting qutebrowser in any other way will not save/restore the
# session. By default, this will save to the session which was last loaded.
# This behavior can be customized via the session.default_name setting.
c.auto_save.session = True

c.fonts.default_size = '13pt'
c.window.hide_decoration = True
c.qt.highdpi = True
