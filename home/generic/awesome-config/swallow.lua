-- Inspired by
-- - https://dwm.suckless.org/patches/swallow
-- - https://www.youtube.com/watch?v=92uo5OBOKfY

local awful = require("awful")

-- Since I'm using tmux messes with process IDs (I can't trace them back to my
-- terminal), I can't do the traditional approach of looking for a window with
-- the right parent id. Instead, what I do is hack it: If I spawned a window
-- from tmux, swallow the previous window (in hope that it's the terminal that
-- executed it)
function on_window_manage(c)
  -- Runs specified callback if this command is running within tmux
  function run_if_inside_tmux (pid, callback)
    awful.spawn.easy_async_with_shell(
      "tmux list-panes -F \"#{pane_pid}\" | grep \"$(ps -o ppid= " .. pid .. ")\"",
      function (_, _, exitreason, exitcode)
        if exitreason == "exit" and exitcode == 0 then
          callback()
        end
      end
    )
  end

  local parent = awful.client.focus.history.get(c.screen, 1)
  if c.pid and parent then
    run_if_inside_tmux(
      c.pid,
      function ()
        -- Minimize
        parent.minimized = true

        -- Unminimize terminal when done
        c:connect_signal(
          "unmanage",
          function (c)
            parent.minimized = false
          end
        )
      end
    )
  end
end

return {
  on_window_manage = on_window_manage
}
