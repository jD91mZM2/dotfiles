import Data.Char
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.StackSet
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce

myModMask  = mod4Mask

confirm :: String -> (X ()) -> X ()
confirm x callback = do
  s <- dmenu ["Cancel", x]
  if s == x then
    callback
  else
    return ()

main = do
  xmonad $ ewmh $ docks $ def
    {
      layoutHook = avoidStruts $ layoutHook def,
      manageHook = composeAll [
        manageHook def,
        className =? "discord"            --> doShift "9",
        className =? "evolution"          --> doShift "9",
        className =? "mattermost-desktop" --> doShift "9"
      ],
      modMask = myModMask,
      startupHook = do
        spawnOnce "~/.dotfiles/xmonad/startup.sh",
      terminal = "konsole"
    }
    `additionalKeys`
    [
      -- PrintScreen
      ((0, xK_Print),                       spawn "~/.dotfiles/screenshot.sh screen"),
      ((myModMask, xK_Print),               spawn "~/.dotfiles/screenshot.sh window"),
      ((myModMask .|. shiftMask, xK_Print), spawn "~/.dotfiles/screenshot.sh region"),

      -- Misc
      ((myModMask .|. shiftMask, xK_l),     spawn "echo -ne '\x2' | socat - UNIX-CONNECT:/tmp/xidlehook.sock"),
      ((myModMask, xK_z),                   spawn $ "echo -n '" ++ [chr 0x200B] ++ "' | xclip -sel clip"),

      -- Overrides
      ((myModMask .|. shiftMask, xK_q),     confirm "Exit XMonad" $ io $ exitWith ExitSuccess),
      ((myModMask .|. shiftMask, xK_r),     spawn "xmonad --recompile"),
      ((myModMask, xK_Down),                windows focusDown),
      ((myModMask, xK_Left),                sendMessage Shrink),
      ((myModMask, xK_Right),               sendMessage Expand),
      ((myModMask, xK_Up),                  windows focusUp),
      ((myModMask, xK_p),                   spawn "j4-dmenu-desktop")
    ]
