-------------------------------------------------------------------------------
-------------------------------- Imports --------------------------------------
import System.IO
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.SetWMName
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Layout.IndependentScreens
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import Data.Maybe (isNothing)
import XMonad.Prompt

import qualified XMonad.StackSet as W

---------------------------------- Functions ----------------------------------
zip4 [] _ _ _ = []
zip4 _ [] _ _ = []
zip4 _ _ [] _ = []
zip4 _ _ _ [] = []
zip4 (w:ws) (x:xs) (y:ys) (z:zs) = [(w,x,y,z)] ++ zip4 ws xs ys zs

------------------------------- Key Bindings ----------------------------------

-- Masks
alt = mod1Mask
ctrl = controlMask
shift = shiftMask
super = mod4Mask

myAdditionalKeys = [ 
   -- Restarts XMonad
     ((alt, xK_r), spawn "xmonad --recompile && xmonad --restart")

   -- Next Layout
   , ((super, xK_space), sendMessage NextLayout)

   -- Remove Current Workspace
   , ((super, xK_r), removeWorkspace)
   , ((ctrl .|. alt .|. shift, xK_n), removeEmptyWorkspace) -- if Empty

   -- Alarm
   , ((super, xK_a), spawn "alarm-xmonad")
   , ((super, xK_s), spawn "alarm-xmonad --stop")

   -- Close Focused Window
   , ((alt, xK_w), spawn "close-window")

   -- Prev/Next Hidden NonEmpty Workspace
   , ((alt, xK_bracketleft), moveTo Prev HiddenNonEmptyWS)
   , ((alt, xK_bracketright), moveTo Next HiddenNonEmptyWS)

   -- Prev/Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((super, xK_bracketleft), sequence_ [nextScreen, moveTo Prev HiddenNonEmptyWS, prevScreen])
   , ((super, xK_bracketright), sequence_ [nextScreen, moveTo Next HiddenNonEmptyWS, prevScreen])

   -- Toggle to Last Workspace
   , ((alt, xK_o), toggleWS)

   -- Prev/Next Tmux Session
   , ((alt, xK_p), spawn "tmux switchc -p")
   , ((alt, xK_n), spawn "tmux switchc -n")

   -- Program Launcher
   , ((alt, xK_space), spawn "dmenu_extended_run")
   , ((alt .|. super, xK_space), sequence_ [addWorkspace "MISC", spawn "dmenu_extended_run"])

   -- Toggle External Monitor
   , ((alt, xK_m), spawn "toggle_monitor")

   -- Screenshot Commands
   , ((alt, xK_Print), spawn "sshot")
   , ((super, xK_Print), spawn "receipt_sshot")

   -- Shutdown
   , ((ctrl .|. super .|. alt, xK_s),
   spawn "confirm -d 'ham stop && dbox_sync && shutdown now'")

   -- Restart
   , ((ctrl .|. super .|. alt, xK_r),
   spawn "confirm -d 'ham stop && systemctl reboot -i'")

   -- Hamster Start and Stop
   , ((super, xK_KP_Delete), spawn "ham start")
   , ((super, xK_KP_Insert), spawn "ham stop")

   -- Tmux Send-Screen Hacks
   , ((alt, xK_e), spawn "clear_screen")
   , ((alt, xK_q), spawn "quit_screen")
   , ((alt, xK_k), spawn "kill_screen")

   -- clipmenu
   , ((alt .|. shift, xK_c), spawn "clipmenu")

   -- screenlock
   , ((super, xK_l), spawn "screenlock")

   -- Focus Local
   , ((alt, xK_f), windows $ W.focusUp)

   -- Next Screen
   , ((alt, xK_backslash), nextScreen)
   , ((alt, xK_Tab), nextScreen)

   -- Swap Screens
   , ((alt, xK_s), sequence_ [swapNextScreen, removeEmptyWorkspace])

   -- Send current WS to Next Screen
   , ((super, xK_slash), sequence_ [swapNextScreen, toggleWS, nextScreen]) -- send focus
   , ((super, xK_backslash), sequence_ [swapNextScreen, toggleWS]) -- don't send focus

   -- Shift current window to MISC
   , ((ctrl, xK_m), sequence_ [addHiddenWorkspace "MISC", windows $ W.shift "MISC", removeEmptyWorkspace, windows $ W.view "MISC"])

   -- Shift current window to _______
   , ((super .|. alt, xK_n), sequence_ [addWorkspacePrompt myXPConfig, setWorkspaceIndex 1, toggleWS, withWorkspaceIndex W.shift 1, removeEmptyWorkspace, withWorkspaceIndex W.view 1])

   -- Create new WS named _______
   , ((super, xK_n), addWorkspacePrompt myXPConfig)

   -- Galculator
   , ((0, xK_F12), spawn "galculator")
   ]

   -- Hamster Numpad Bindings
   ++ [((super, key), spawn $ "ham start " ++ (show i))
       | (i, key) <- zip [1 .. 5] [xK_KP_End, xK_KP_Down, xK_KP_Page_Down, xK_KP_Left, xK_KP_Begin]
      ]

   -- Launch Applications
   ++ [((alt, key), raiseNextMaybe (sequence_ [addWorkspace ws, (spawnOn ws $ cmd)]) (className =? cls))
       | (key, cmd, cls, ws) <- zip4
       [xK_x, xK_c, xK_z, xK_a, xK_1, xK_2]
       ["termite -e 'tm-init Terminal'","google-chrome-stable","zathura","anki","hamster","slack"]
       ["Termite","Google-chrome","Zathura","Anki","Hamster","Slack"]
       ["TERM","WEB","PDF","ANKI","HAMSTER","SLACK"]
      ]

   -- Raise or Run Second Instance of an Application
   ++ [((super, key), sequence_ [nextScreen, addWorkspace ws, spawnOn ws ("WS_is_Empty && " ++ cmd)])
       | (key,cmd,ws) <- zip3 
       [xK_c,xK_z]
       ["google-chrome-stable","zathura"]
       ["WEB2","PDF2"]
      ]

   -- Shift; Focus
   ++ [((ctrl, k), sequence_ [withNthWorkspace W.shift i, withNthWorkspace W.view i])
       | (i, k) <- zip [0..9] $ [xK_1 .. xK_9] ++ [xK_0]
      ]

   -- View Workspace
   ++ [((super, k), withNthWorkspace W.view i)
       | (i, k) <- zip [0..9] $ [xK_1 .. xK_9] ++ [xK_0]
      ]

-------------------------------- Misc Configs ---------------------------------
myTerminal = "termite -e 'tm-init Terminal'"
myModMask = mod1Mask

myFocusFollowsMouse = False
myClickJustFocuses = False

-- Colors --
yellow = "#F8FB27"
red = "#FF0000"
blue = "#0000FF"
------------

myBorderWidth = 5
myFocusedBorderColor = blue

myWorkspaces :: [String]
myWorkspaces = ["TERM","WEB"]

myXPConfig :: XPConfig
myXPConfig = def {position = Bottom}

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = smartSpacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook = composeAll
    [ manageSpawn
    , className=? "Galculator"      --> doFloat
    , className=? "Pinentry"        --> doFloat]

myStartupHook = ewmhDesktopsStartup
                >> setWMName "LG3D"
                >> spawn "init-bg"
                >> spawn "maintCheck"
                >> spawn "sleep 3 && volume-xmonad"
                >> spawn "alarm-xmonad --resume"

-------------------------------- Main -----------------------------------------
main = do
    xmproc <- spawnPipe "xmobar /home/bryan/.xmobarrc"
    xmonad $ ewmh desktopConfig
        {
            terminal                = myTerminal
          , modMask                 = myModMask
          , borderWidth             = myBorderWidth
          , focusedBorderColor      = myFocusedBorderColor
          , focusFollowsMouse       = myFocusFollowsMouse
          , clickJustFocuses        = myClickJustFocuses
          , workspaces              = myWorkspaces
          , manageHook              = myManageHook
          , layoutHook              = avoidStruts $ myLayout
          , startupHook             = myStartupHook
          , logHook                 = dynamicLogWithPP xmobarPP
            { ppOutput                = hPutStrLn xmproc
            , ppOrder                 = \(ws:l:t:_)   -> [ws]
            , ppCurrent               = xmobarColor "yellow" "" . wrap "[" "]"
            , ppHidden                = xmobarColor "white" ""
            , ppHiddenNoWindows       = xmobarColor "darkgrey" ""
            , ppWsSep                 = "    "
            , ppTitle                 = xmobarColor "green"  "" . shorten 40
            , ppVisible               = xmobarColor "yellow" ""
            , ppUrgent                = xmobarColor "red" "yellow"
            } >> ewmhDesktopsLogHook <+> dynamicLogXinerama
      } `additionalKeys` myAdditionalKeys
