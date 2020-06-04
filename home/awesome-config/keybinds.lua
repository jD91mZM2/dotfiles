local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")

--[[
REMAINING KEYBINDINGS FROM SXHKD

# lock the screen with xidlehook
super + shift + l
    xidlehook-client --socket /tmp/xidlehook.sock control --action trigger --timer 1

# volume settings
XF86Audio{Raise,Lower}Volume
    ~/dotfiles/scripts/volume.perl {up,down}
XF86AudioMute
    pactl set-sink-mute "@DEFAULT_SINK@" toggle

# Quick clipboard
F1; a
    cat ~/dotfiles/data/ascii | ~/dotfiles/scripts/dmenu.sh -p "ASCII:" | sed "s/^.*=\s*//" | tr -d $'\n' | xclip -sel clip
F1; e
    cat ~/dotfiles/data/emoji | ~/dotfiles/scripts/dmenu.sh -p "Emoji:" | sed "s/^.*=\s*//" | tr -d $'\n' | xclip -sel clip

# secret??
super + Up; Up; Down; Down; Left; Right; Left; Right; b; a
    xdg-open "https://youtu.be/dQw4w9WgXcQ"
]]

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Global keybindings
globalkeys = gears.table.join(
  awful.key({ modkey }, "s", hotkeys_popup.show_help, { description = "show help", group = "awesome" }),

  awful.key({ modkey }, "j", function () awful.client.focus.byidx( 1) end, { description = "focus next by index", group = "client" }),
  awful.key({ modkey }, "k", function () awful.client.focus.byidx(-1) end, { description = "focus previous by index", group = "client" }),
  awful.key({ modkey }, "w", function () mymainmenu:show() end, { description = "show main menu", group = "awesome" }),

  -- Layout manipulation
  awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(  1) end, { description = "swap with next client by index", group = "client" }),
  awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx( -1) end, { description = "swap with previous client by index", group = "client" }),
  awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end, { description = "focus the next screen", group = "screen" }),
  awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end, { description = "focus the previous screen", group = "screen" }),
  awful.key({ modkey }, "u", awful.client.urgent.jumpto, { description = "jump to urgent client", group = "client" }),
  awful.key({ modkey }, "Tab",
    function ()
      awful.client.focus.history.previous()
      if client.focus then
        client.focus:raise()
      end
    end,
    { description = "go back", group = "client" }),

  -- Standard program
  awful.key({ modkey, "Shift" }, "Return", function () awful.spawn("st -e tmux") end, { description = "open a terminal", group = "launcher" }),
  awful.key({ modkey, "Control" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
  awful.key({ modkey }, "Pause",
    function () awful.spawn.with_shell("~/dotfiles/scripts/dmenu-confirm.sh Shutdown && systemctl poweroff") end,
    { description = "turn off computer", group = "awesome" }),
  awful.key({ modkey, "Shift" }, "q",
    function () awful.spawn.with_shell("~/dotfiles/scripts/dmenu-confirm.sh \"Exit AwesomeWM\" && awesome-client \"quit()\"") end,
    { description = "quit awesome", group = "awesome" }),

  -- Switch between layouts
  awful.key({ modkey }, "t", function () awful.layout.set(awful.layout.suit.spiral) end, { description = "tiling view", group = "layout" }),
  awful.key({ modkey }, "n", function () awful.layout.set(awful.layout.suit.magnifier) end, { description = "magnifier view", group = "layout" }),
  awful.key({ modkey }, "m", function () awful.layout.set(awful.layout.suit.max) end, { description = "stacked view", group = "layout" }),

  -- Screenshot
  awful.key({}, "Print", function () awful.spawn.with_shell("~/dotfiles/scripts/screenshot.sh screen") end, { description = "dump screen", group = "screenshot" }),
  awful.key({ modkey }, "Print", function () awful.spawn.with_shell("~/dotfiles/scripts/screenshot.sh window") end, { description = "dump window", group = "screenshot" }),
  awful.key({ modkey, "Shift" }, "Print", function () awful.spawn.with_shell("~/dotfiles/scripts/screenshot.sh region") end, { description = "dump region", group = "screenshot" }),

  -- Misc
  awful.key({ modkey }, "p", function () awful.spawn("j4-dmenu-desktop --dmenu \"~/dotfiles/scripts/dmenu.sh -p 'Execute:'\"") end, { description = "show the menubar", group = "misc" }),
  awful.key({ modkey }, "z", function () awful.spawn.with_shell("echo -n \"\xE2\x80\x8B\" | xclip -sel clip") end, { description = "clear the clipboard (zero-width space)", group = "misc" })
)
-- Global mouse-bindings
globalbuttons = gears.table.join(
  awful.button({}, 4, awful.tag.viewnext),
  awful.button({}, 5, awful.tag.viewprev)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
  globalkeys = gears.table.join(
    -- Previous
    globalkeys,

    -- View tag only.
    awful.key({ modkey }, "#" .. i + 9,
      function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
          tag:view_only()
        end
      end,
      { description = "view tag #"..i, group = "tag" }),

    -- Toggle tag display.
    awful.key({ modkey, "Control" }, "#" .. i + 9,
      function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
          awful.tag.viewtoggle(tag)
        end
      end,
      { description = "toggle tag #" .. i, group = "tag" }),

    -- Move client to tag.
    awful.key({ modkey, "Shift" }, "#" .. i + 9,
      function ()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:move_to_tag(tag)
          end
        end
      end,
      { description = "move focused client to tag #"..i, group = "tag" }),

    -- Toggle tag on focused client.
    awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
      function ()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:toggle_tag(tag)
          end
        end
      end,
      { description = "toggle focused client on tag #" .. i, group = "tag" })
  )
end

-- Window-specific keybindings
clientkeys = gears.table.join(
  awful.key({ modkey }, "f",
    function (c)
      c.fullscreen = not c.fullscreen
      c:raise()
    end,
    { description = "toggle fullscreen", group = "client" }),
  awful.key({ modkey, "Shift" }, "c", function (c) c:kill() end, { description = "close", group = "client" }),
  awful.key({ modkey, "Control" }, "space", awful.client.floating.toggle, { description = "toggle floating", group = "client" }),
  awful.key({ modkey }, "Return", function (c) c:swap(awful.client.getmaster()) end, { description = "move to master", group = "client" }),
  awful.key({ modkey }, "o", function (c) c:move_to_screen() end, { description = "move to screen", group = "client" })
)

-- Window-specific mouse-bindings
clientbuttons = gears.table.join(
  awful.button({}, 1, function (c)
      c:emit_signal("request::activate", "mouse_click", { raise = true })
  end),
  awful.button({ modkey }, 1, function (c)
      c:emit_signal("request::activate", "mouse_click", { raise = true })
      awful.mouse.client.move(c)
  end),
  awful.button({ modkey }, 3, function (c)
      c:emit_signal("request::activate", "mouse_click", { raise = true })
      awful.mouse.client.resize(c)
  end)
)

return {
  globalkeys = globalkeys,
  globalbuttons = globalbuttons,
  clientkeys = clientkeys,
  clientbuttons = clientbuttons,
}
