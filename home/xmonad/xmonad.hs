import Data.Char
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.StackSet
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes

myWorkspaces = map show [1..9]

xK_XF86AudioLowerVolume = 0x1008ff11
xK_XF86AudioRaiseVolume = 0x1008ff13

myModMask = mod4Mask
myDmenu = "~/dotfiles/scripts/dmenu.sh"

confirm :: String -> (X ()) -> X ()
confirm x callback = do
  s <- menuArgs myDmenu [] ["Cancel", x]
  if s == x
    then callback
    else return ()

join :: [String] -> String
join [] = ""
join (x:xs) = "\"" ++ x ++ "\" " ++ join xs

ws :: Int -> String
ws n = myWorkspaces !! (n-1)

main = xmonad $ ewmh $ fullscreenSupport $ docks
    def {
      layoutHook =
        avoidStruts $

        gaps [(U, 6), (L, 6), (D, 6), (R, 6)] $
        spacing 2 $

        toggleLayouts Full $

        onWorkspace (ws 9) (tabbedLeft shrinkText $ theme deiflTheme) $
        layoutHook def,
      manageHook = composeAll [
        manageHook def,
        appName =? "chat.redox-os.org" <||>
          appName =? "discordapp.com__channels_@me" <||>
          appName =? "liferea" <||>
          appName =? "qemu-system-x86_64" <||>
          appName =? "Mail"
          --> doShift (ws 9)
      ],
      modMask = myModMask,
      startupHook =
        spawnOnce "~/dotfiles/scripts/startup.sh",
      terminal = "st",
      XMonad.workspaces = myWorkspaces
    }
    `additionalKeys`
    [
      -- PrintScreen
      ((0, xK_Print),                       spawn "~/dotfiles/scripts/screenshot.sh screen"),
      ((myModMask, xK_Print),               spawn "~/dotfiles/scripts/screenshot.sh window"),
      ((myModMask .|. shiftMask, xK_Print), spawn "~/dotfiles/scripts/screenshot.sh region"),

      -- Volume
      ((0, xK_XF86AudioLowerVolume), spawn "~/dotfiles/scripts/volume.perl down"),
      ((0, xK_XF86AudioRaiseVolume), spawn "~/dotfiles/scripts/volume.perl up"),

      -- Misc
      ((myModMask .|. shiftMask, xK_l),       spawn "echo -n '\x2' | socat - UNIX-CONNECT:/tmp/xidlehook.sock"),
      ((myModMask, xK_z),                     spawn "echo -n '\x200b' | xclip -sel clip"),
      ((myModMask, xK_f),                     withFocused $ sendMessage . AddFullscreen),
      ((myModMask .|. shiftMask, xK_f),       withFocused $ sendMessage . RemoveFullscreen),
      ((myModMask .|. controlMask, xK_space), sendMessage ToggleLayout),

      -- Arrow keys aren't evil
      ((myModMask, xK_Left),  sendMessage Shrink),
      ((myModMask, xK_Right), sendMessage Expand),
      ((myModMask, xK_Down),  windows focusDown),
      ((myModMask, xK_Up),    windows focusUp),
      ((myModMask .|. shiftMask, xK_Down), windows swapDown),
      ((myModMask .|. shiftMask, xK_Up),   windows swapUp),

      -- System
      ((myModMask .|. shiftMask, xK_q), confirm "Exit XMonad" $ io $ exitWith ExitSuccess),
      ((myModMask, xK_Pause),           confirm "Shutdown" $ spawn "systemctl poweroff"),

      ((myModMask .|. shiftMask, xK_r), spawn "xmonad --recompile && notify-send -t 1000 'Recompiled!' 'xmonad was successfully recompiled'"),
      ((myModMask, xK_p),               spawn $ "j4-dmenu-desktop --dmenu '" ++ (myDmenu) ++ "'")
    ]
