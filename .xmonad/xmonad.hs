-------------------------------------------------------------------------------
-------------------------------- Imports --------------------------------------
import System.IO
import Data.Maybe (isNothing)
import Control.Monad (liftM)
import Graphics.X11.ExtraTypes.XF86

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
import XMonad.Prompt
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W
import qualified XMonad.Actions.DynamicWorkspaceOrder as DW

---------------------------------- Functions ----------------------------------
-- Function that prevents cycling to workspaces available on other screens
hiddenNotNSP :: X (WindowSpace -> Bool)
hiddenNotNSP = do
  hs <- gets $ map W.tag . W.hidden . windowset
  return (\w -> (W.tag w) /= "NSP" && (W.tag w) `elem` hs)

-- Used to filter out NSP from xmobar
noScratchPad ws = if ws == "NSP" then "" else ws

-- | This is a re-implementation of DW.withNthworkspace with "skipTags"
-- added to filter out NSP.
withNthWorkspace' :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace' job wnum = do
    sort <- DW.getSortByOrder
    ws <- gets (map W.tag . sort . namedScratchpadFilterOutWorkspace . W.workspaces . windowset)
    case drop wnum ws of
        (w:_) -> windows $ job w
        []    -> return ()
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
   , ((alt .|. super, xK_space), sendMessage NextLayout)

   -- Remove Current Workspace
   , ((super, xK_r), removeWorkspace)
   , ((ctrl .|. alt .|. shift, xK_n), removeEmptyWorkspace) -- if Empty

   -- Alarm
   , ((super, xK_a), spawn "alarm-xmonad")
   , ((super .|. shift, xK_a), spawn "alarm-xmonad --stop && sleep 0.5 && xdotool key ctrl+space")

   -- Scratchpad
   , ((super, xK_s), namedScratchpadAction scratchpads "scratchpad")
   , ((0, xF86XK_Calculator), namedScratchpadAction scratchpads "calculator")
   , ((ctrl .|. super, xK_c), namedScratchpadAction scratchpads "calculator")

   -- Close Focused Window
   , ((alt, xK_w), spawn "close-window")

   -- Prev/Next Hidden NonEmpty Workspace
   , ((alt, xK_bracketleft), moveTo Prev (WSIs hiddenNotNSP))
   , ((alt, xK_bracketright), moveTo Next (WSIs hiddenNotNSP))

   -- Prev/Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((super, xK_bracketleft), sequence_ [nextScreen, moveTo Prev (WSIs hiddenNotNSP), prevScreen])
   , ((super, xK_bracketright), sequence_ [nextScreen, moveTo Next (WSIs hiddenNotNSP), prevScreen])

   -- Toggle to Last Workspace
   , ((super, xK_o), toggleWS' ["NSP"])

   -- Prev/Next Tmux Session/Window
   , ((alt, xK_9), spawn "tmux switchc -p")
   , ((alt, xK_0), spawn "tmux switchc -n")
   , ((alt, xK_p), spawn "tmux next-window")
   , ((alt, xK_n), spawn "tmux previous-window")

   -- Program Launcher
   , ((alt, xK_space), spawn "dmenu_extended_run")
   , ((super, xK_space), sequence_ [addWorkspace "MISC", spawn "dmenu_extended_run"])

   -- Open New Book in Okular
   , ((alt, xK_o), spawn "dmenu_books --application=okular")

   -- Toggle External Monitor
   , ((alt, xK_m), spawn "toggle_monitor")

   -- Screenshot Commands
   , ((alt, xK_Print), spawn "sshot")
   , ((super, xK_Print), spawn "receipt_sshot")

   -- Shutdown
   , ((ctrl .|. super .|. alt, xK_s),
   spawn "confirm --dmenu 'ham stop && dbox_sync && shutdown now'")

   -- Restart
   , ((ctrl .|. super .|. alt, xK_r),
   spawn "confirm --dmenu 'ham stop && systemctl reboot -i'")

   -- Hamster Start and Stop
   , ((super, xK_KP_Delete), spawn "ham start")
   , ((super, xK_KP_Insert), spawn "ham stop")

   -- 'tmux send-keys' Hacks
   , ((alt, xK_e), spawn "tm-send --action=clear")
   , ((alt, xK_q), spawn "tm-send --action=quit")
   , ((alt, xK_k), spawn "tm-kill")
   , ((alt, xK_h), spawn "tmux send-keys 'cd && clear' 'Enter'")


   -- clipmenu
   , ((ctrl .|. alt, xK_c), spawn "clipmenu")

   -- screenlock
   , ((super, xK_l), spawn "screenlock")

   -- Local WS Commands
   , ((alt, xK_f), windows $ W.focusUp)     -- Focus
   , ((super, xK_f), windows W.swapDown)    -- Shift

   -- Next Screen
   , ((alt, xK_backslash), nextScreen)
   , ((alt, xK_Tab), nextScreen)

   -- Swap
   , ((alt, xK_s), sequence_ [swapNextScreen, spawn "removeEmptyWorkspace"])

   -- Expand or Shrink Master Area
   , ((super, xK_j), sendMessage Shrink)
   , ((super, xK_k), sendMessage Expand)

   -- Send current WS to Next Screen
   , ((super, xK_slash), sequence_ [swapNextScreen, toggleWS' ["NSP"], nextScreen]) -- send focus
   , ((super, xK_backslash), sequence_ [swapNextScreen, toggleWS' ["NSP"]]) -- don't send focus

   -- Shift current window to MISC
   , ((super, xK_m), sequence_ [addHiddenWorkspace "MISC", windows $ W.shift "MISC", removeEmptyWorkspace, windows $ W.view "MISC"])

   -- Shift current window to _______
   , ((super, xK_n), sequence_ [addWorkspacePrompt myXPConfig, setWorkspaceIndex 1, toggleWS' ["NSP"], withWorkspaceIndex W.shift 1, removeEmptyWorkspace, withWorkspaceIndex W.view 1])
   ]

   -- Hamster Numpad Bindings
   ++ [((super, key), spawn $ "ham start " ++ (show i))
       | (i, key) <- zip [1..9] [xK_KP_End,xK_KP_Down,xK_KP_Page_Down,xK_KP_Left,xK_KP_Begin,xK_KP_Right,xK_KP_Home,xK_KP_Up,xK_KP_Page_Up]
      ]

   -- Launch Applications
   ++ [((alt, key), sequence_ [addWorkspace ws, (spawnOn ws $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [xK_x, xK_c, xK_z, xK_v, xK_a, xK_1, xK_2]
       ["termite -e 'tm-init Terminal'","google-chrome-stable","zathura","okular","anki","hamster","slack"]
       ["TERM","WEB","ZATH","OKULAR","ANKI","HAMSTER","SLACK"]
      ]

   -- Launch Second Applications
   ++ [((super, key), sequence_ [addWorkspace ws, (spawnOn ws $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [xK_c, xK_z]
       ["google-chrome-stable", "zathura"]
       ["WEB'", "ZATH'"]
      ]

   -- Shift to WS; then Focus WS
   ++ [((super, k), sequence_ [withNthWorkspace' W.shift i, removeEmptyWorkspace, withNthWorkspace' W.view i])
       | (i, k) <- zip [0..9] $ [xK_1 .. xK_9] ++ [xK_0]
      ]

   -- View WS
   ++ [((ctrl, k), withNthWorkspace' W.view i)
       | (i, k) <- zip [0..9] $ [xK_1 .. xK_9] ++ [xK_0]
      ]

-------------------------------- Misc Configs ---------------------------------
myTerminal = "termite"
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

scratchpads = [ NS "scratchpad" scratchpad (role =? "scratchpad") 
                    (customFloating $ W.RationalRect l t w h)
              , NS "calculator" "galculator" (className =? "Galculator")
                    (customFloating $ W.RationalRect l t w h)]
            where 
                role = stringProperty "WM_WINDOW_ROLE"
                scratchpad = "termite -r scratchpad --class scratchpad -d ~/Dropbox/notes" 
                h = 0.5
                w = 0.5
                t = 0.4  -- Distance from top edge
                l = 0.25

myManageHook = composeAll
    [ manageSpawn
    , namedScratchpadManageHook scratchpads
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
            , ppSort                  = (namedScratchpadFilterOutWorkspace .) `liftM` getSortByIndex
            } >> ewmhDesktopsLogHook <+> dynamicLogXinerama
      } `additionalKeys` myAdditionalKeys
