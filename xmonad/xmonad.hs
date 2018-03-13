import Data.Char
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace
import XMonad.StackSet
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

xK_XF86AudioLowerVolume = 0x1008ff11
xK_XF86AudioRaiseVolume = 0x1008ff13

myModMask  = mod4Mask

confirm :: String -> (X ()) -> X ()
confirm x callback = do
  s <- dmenu ["Cancel", x]
  if s == x then
    callback
  else
    return ()

main = do
  xmonad $ ewmh $ docks $ fullscreenSupport def
    {
      layoutHook =
        avoidStruts $
        gaps [(U, 6), (L, 6), (D, 6), (R, 6)] $
        spacing 2 $
        onWorkspace "9" (Tall 1 (3/100) (17/20)) $
        layoutHook def,
      manageHook = composeAll [
        manageHook def,
        appName =? "discord" <||>
        appName =? "evolution" <||>
        appName =? "liferea" <||>
        appName =? "mattermost" --> doShift "9"
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

      -- Volume
      ((0, xK_XF86AudioLowerVolume), spawn "amixer set Master 2%- unmute && \
\ paplay /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga"),
      ((0, xK_XF86AudioRaiseVolume), spawn "amixer set Master 2%+ unmute && \
\ paplay /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga"),

      -- Misc
      ((myModMask .|. shiftMask, xK_l), spawn "echo -ne '\x2' | socat - UNIX-CONNECT:/tmp/xidlehook.sock"),
      ((myModMask, xK_z),               spawn $ "echo -n '" ++ [chr 0x200B] ++ "' | xclip -sel clip"),
      ((myModMask, xK_f),               withFocused (\x -> sendMessage $ AddFullscreen x)),
      ((myModMask .|. shiftMask, xK_f), withFocused (\x -> sendMessage $ RemoveFullscreen x)),

      -- Arrow keys aren't evil
      ((myModMask, xK_Down),  windows focusDown),
      ((myModMask, xK_Left),  sendMessage Shrink),
      ((myModMask, xK_Right), sendMessage Expand),
      ((myModMask, xK_Up),    windows focusUp),

      -- System
      ((myModMask .|. shiftMask, xK_q), confirm "Exit XMonad" $ io $ exitWith ExitSuccess),
      ((myModMask, xK_Pause),           confirm "Shutdown" $ spawn "systemctl poweroff"),

      ((myModMask .|. shiftMask, xK_r), spawn "xmonad --recompile"),
      ((myModMask, xK_p),               spawn "j4-dmenu-desktop")
    ]
