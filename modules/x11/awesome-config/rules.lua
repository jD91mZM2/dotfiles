local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local naughty = require("naughty")

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
        "pinentry",
      },
      class = {
        "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
      },

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
    rule = { instance = "neovim" },
    properties = { screen = 1, tag = "2" }
  },
  {
    rule = { class = "Firefox" },
    properties = { screen = 1, tag = "3" }
  },
  {
    rule_any = {
      class = { "Thunderbird" },
      instance = { "weechat" },
      name = { ".*Redox Mattermost.*", ".*Discord.*" },
    },
    properties = { screen = 1, tag = "9" }
  },
}

-- Apply rules to each client after they've all properly started (hacky)
-- I do this because firefox doesn't set the title bar until it's properly started.

gears.timer.start_new(
  10, function()
    for _, c in ipairs(client.get()) do
      -- polybar can't be updated after launch for some reason, it gets moved
      -- in a weird way
      if c.instance ~= "polybar" then
        awful.rules.apply(c)
      end
    end

    return false
  end
)
