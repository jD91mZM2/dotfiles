local awful = require("awful")
local beautiful = require("beautiful")

-- Arguments
local keybinds=...

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
  -- All clients will match this rule.
  {
    rule = { },
    properties = {
      border_color = beautiful.border_normal,
      border_width = beautiful.border_width,
      buttons = keybinds.clientbuttons,
      focus = awful.client.focus.filter,
      keys = keybinds.clientkeys,
      placement = awful.placement.no_overlap+awful.placement.no_offscreen,
      raise = true,
      screen = awful.screen.preferred,
      size_hints_honor = false,
    },
  },

  -- Floating clients.
  {
    rule_any = {
      instance = {
        "DTA",  -- Firefox addon DownThemAll.
        "copyq",  -- Includes session name in class.
        "pinentry",
      },
      class = {
        "Arandr",
        "Blueman-manager",
        "Gpick",
        "Kruler",
        "MessageWin",  -- kalarm.
        "Sxiv",
        "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
        "Wpa_gui",
        "veromix",
        "xtightvncviewer"},

      -- Note that the name property shown in xprop might be set slightly after creation of the client
      -- and the name shown there might not match defined rules here.
      name = {
        "Event Tester",  -- xev.
      },
      role = {
        "AlarmWindow",  -- Thunderbird's calendar.
        "ConfigManager",  -- Thunderbird's about:config.
      }
    },
    properties = { floating = true },
  },

  {
    rule = { class = "Emacs" },
    properties = { screen = 1, tag = "2" }
  },
  {
    rule = { class = "Firefox" },
    properties = { screen = 1, tag = "3" }
  },
  {
    rule_any = {
      class = { "Chromium-browser", "Daily" },
      name = { "weechat" },
    },
    properties = { screen = 1, tag = "9" }
  },
}
