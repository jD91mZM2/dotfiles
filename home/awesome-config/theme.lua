---------------------------
-- Default awesome theme --
---------------------------

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

-- Include default theme
local theme = loadfile(themes_path .. "default/theme.lua")()

theme.bg_normal     = "#282a36"
theme.bg_focus      = "#44475a"
theme.bg_urgent     = "#ff5555"
theme.bg_systray    = theme.bg_normal

theme.wallpaper = "~/Pictures/background.jpg"

theme.fg_normal     = "#6272a4"
theme.fg_focus      = "#f8f8f2"
theme.fg_urgent     = "#6272a4"

theme.padding       = 5
theme.useless_gap   = 1
theme.border_width  = 1
theme.border_normal = "#282a36"
theme.border_focus  = "#44475a"
theme.border_marked = "#ffb86c"

return theme
