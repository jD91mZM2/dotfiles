local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Not-stupid focus that doesn't disappear at times
require("awful.autofocus")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require("awful.hotkeys_popup.keys")

--  _____                       _                     _ _ _
-- | ____|_ __ _ __ ___  _ __  | |__   __ _ _ __   __| | (_)_ __   __ _
-- |  _| | '__| '__/ _ \| '__| | '_ \ / _` | '_ \ / _` | | | '_ \ / _` |
-- | |___| |  | | | (_) | |    | | | | (_| | | | | (_| | | | | | | (_| |
-- |_____|_|  |_|  \___/|_|    |_| |_|\__,_|_| |_|\__,_|_|_|_| |_|\__, |
--                                                                |___/

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify {
    preset = naughty.config.presets.critical,
    title = "Oops, there were errors during startup!",
    text = awesome.startup_errors,
  }
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal(
    "debug::error",
    function (err)
      -- Make sure we don't go into an endless error loop
      if in_error then return end
      in_error = true

      naughty.notify {
        preset = naughty.config.presets.critical,
        title = "Oops, an error happened!",
        text = tostring(err),
      }
      in_error = false
    end
  )
end

--  _                    _         _   _                  __ _ _
-- | |    ___   __ _  __| |   ___ | |_| |__   ___ _ __   / _(_) | ___  ___
-- | |   / _ \ / _` |/ _` |  / _ \| __| '_ \ / _ \ '__| | |_| | |/ _ \/ __|
-- | |__| (_) | (_| | (_| | | (_) | |_| | | |  __/ |    |  _| | |  __/\__ \
-- |_____\___/ \__,_|\__,_|  \___/ \__|_| |_|\___|_|    |_| |_|_|\___||___/

function try_load(filepath, ...)
  local result, err = loadfile(gears.filesystem.get_configuration_dir() .. filepath)
  if err then
    naughty.notify {
      preset = naughty.config.presets.critical,
      title = "Error loading file",
      text = err,
    }
    return nil
  end
  return result(...)
end

local theme = try_load("theme.lua")

-- Setup keybinds early on
local keybinds = try_load("keybinds.lua")
root.keys(keybinds.globalkeys)
root.buttons(keybinds.globalbuttons)

-- Themes define colours, icons, font and wallpapers.
if not theme or not beautiful.init(theme) then
  naughty.notify {
    preset = naughty.config.presets.critical,
    text = "Failed to load theme"
  }

  -- Fallback theme
  beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
end

local function set_wallpaper(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

--  ____       _
-- / ___|  ___| |_ _   _ _ __    ___  ___ _ __ ___  ___ _ __  ___
-- \___ \ / _ \ __| | | | '_ \  / __|/ __| '__/ _ \/ _ \ '_ \/ __|
--  ___) |  __/ |_| |_| | |_) | \__ \ (__| | |  __/  __/ | | \__ \
-- |____/ \___|\__|\__,_| .__/  |___/\___|_|  \___|\___|_| |_|___/
--                      |_|

local monocle = try_load("monocle.lua")

awful.screen.connect_for_each_screen(
  function (screen)
    -- Set padding
    screen.padding = beautiful.padding;

    -- Wallpaper
    set_wallpaper(screen)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8" }, screen, awful.layout.suit.spiral)
    awful.tag.add(
      "9",
      {
        screen = screen,
        layout = awful.layout.suit.max,
      }
    )

    -- Add cool sidebar widget for monocle screens
    if monocle then
      monocle.init_screen(screen)
    end
  end
)

--  ____       _                          _           _
-- / ___|  ___| |_ _   _ _ __   __      _(_)_ __   __| | _____      _____
-- \___ \ / _ \ __| | | | '_ \  \ \ /\ / / | '_ \ / _` |/ _ \ \ /\ / / __|
--  ___) |  __/ |_| |_| | |_) |  \ V  V /| | | | | (_| | (_) \ V  V /\__ \
-- |____/ \___|\__|\__,_| .__/    \_/\_/ |_|_| |_|\__,_|\___/ \_/\_/ |___/
--                      |_|

local rules = try_load("rules.lua", keybinds)

-- Signal function to execute when a new client appears.
client.connect_signal(
  "manage",
  function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
      -- Prevent clients from being unreachable after screen count changes.
      awful.placement.no_offscreen(c)
    end
  end
)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
  "mouse::enter",
  function (c)
    c:emit_signal("request::activate", "mouse_enter", { raise = false })
  end
)

client.connect_signal("focus", function (c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function (c) c.border_color = beautiful.border_normal end)

--  __  __ _
-- |  \/  (_)___  ___
-- | |\/| | / __|/ __|
-- | |  | | \__ \ (__
-- |_|  |_|_|___/\___|

-- Restart buggy polybar
awful.spawn("systemctl restart --user polybar")
