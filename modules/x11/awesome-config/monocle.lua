local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

function init_screen(screen)
  -- Set up a vertical wibar for tasklist
  screen.monocle_tasklist = awful.wibar({
      position = "left",
      screen = screen,
      visible = false,
      width = 200,
  })
  screen.monocle_tasklist:setup {
    layout = wibox.layout.stack,
    {
      awful.widget.tasklist {
        screen  = screen,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = gears.table.join(
          awful.button({}, 1, function (c)
              c:emit_signal(
                "request::activate",
                "tasklist",
                { raise = true }
              )
          end),
          awful.button({}, 4, function ()
              awful.client.focus.byidx(1)
          end),
          awful.button({}, 5, function ()
              awful.client.focus.byidx(-1)
          end)
        ),
        layout = wibox.layout.fixed.vertical(),

        widget_template = {
          {
            {
              id     = "text_role",
              widget = wibox.widget.textbox,
            },
            widget = wibox.container.margin,
            top    = 5,
            bottom = 5,
          },
          id     = "background_role",
          widget = wibox.container.background,
        },
      },
      widget = wibox.container.margin,
      top = 50,
    },
  }

  for _, t in ipairs(screen.tags) do
    t.display_mono = true

    local callback = function(tag)
      screen.monocle_tasklist.visible = (tag.display_mono and tag.layout == awful.layout.suit.max)
    end

    t:connect_signal("property::selected",     callback)
    t:connect_signal("property::display_mono", callback)
    t:connect_signal("property::layout",       callback)
  end
end

return {
  init_screen = init_screen,
}
