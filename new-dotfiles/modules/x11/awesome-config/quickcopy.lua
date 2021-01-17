local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")

local scripts = "${XDG_DATA_HOME:-$HOME/.local/share}/scripts/"

local popup
local extract = "sed 's/^.*=\\s*//' | tr -d $'\\n' | xclip -sel clip"
awful.keygrabber {
  root_keybindings = {
    {
      {}, "F1",
      function ()
      end,
      { description = "emoji / ascii art", group = "misc" },
    }
  },
  allowed_keys = { "F1", "a", "e" },
  keybindings = {
    {{}, "a", function (self) awful.spawn.with_shell("cat ~/dotfiles/data/ascii | " .. scripts .. "/dmenu.sh -p 'ASCII:' | " .. extract); self:stop() end},
    {{}, "e", function (self) awful.spawn.with_shell("cat ~/dotfiles/data/emoji | " .. scripts .. "/dmenu.sh -p 'Emoji:' | " .. extract); self:stop() end},
  },
  start_callback = function ()
    popup = awful.popup {
      widget = {
        {
          {
            widget = wibox.widget.textbox,
            text = "e",
            align = "center",
            font = "Hack 18",
          },
          {
            widget = wibox.widget.textbox,
            text = "a",
            align = "center",
            font = "Hack 18",
          },
          layout = wibox.layout.flex.horizontal,
          spacing = 3,
          spacing_widget = wibox.widget.separator,
        },
        widget = wibox.container.constraint,
        forced_width = 400,
        forced_height = 200,
      },
      placement = awful.placement.centered,
      ontop = true,
      visible = true,
    }
  end,
  stop_callback = function ()
    popup.visible = false
  end,
}
