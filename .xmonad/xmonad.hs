-------------------------------------------------------------------------------
-------------------------------- Imports --------------------------------------
import XMonad
import XMonad.Actions.SpawnOn (spawnOn,manageSpawn)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhDesktopsLogHook,ewmhDesktopsStartup)

import Data.Maybe (isNothing)
import Control.Monad (liftM)
import Graphics.X11.ExtraTypes.XF86
import Network.HostName (getHostName)

import qualified XMonad.StackSet as W
import qualified XMonad.Prompt as P
import qualified XMonad.Util.NamedScratchpad as NSP
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Actions.CycleWS as CW
import qualified XMonad.Actions.DynamicWorkspaces as DW
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
    ws <- gets (map W.tag . sort . NSP.namedScratchpadFilterOutWorkspace . W.workspaces . windowset)
    case drop wnum ws of
        (w:_) -> windows $ job w
        []    -> return ()

getXmobarTemplate :: String -> String
getXmobarTemplate "athena" = "--template=\"%UnsafeStdinReader% }%hamster%{ %alarm%%dynnetwork%  |  %dropbox%  |  %volume%  |  %date%\""
getXmobarTemplate "aphrodite" = "--template=\"%UnsafeStdinReader% }%hamster%{ %alarm%%dynnetwork%  |  %dropbox%  |  %battery%  |  %volume%  |  %date%\""
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
   , ((super, xK_r), DW.removeWorkspace)
   , ((ctrl .|. alt .|. shift, xK_n), DW.removeEmptyWorkspace) -- if Empty

   -- Alarm
   , ((super, xK_a), spawn "alarm-xmonad")

   -- Scratchpad
   , ((super, xK_s), NSP.namedScratchpadAction scratchpads "scratchpad")
   , ((0, xF86XK_Calculator), NSP.namedScratchpadAction scratchpads "calculator")
   , ((ctrl .|. super, xK_c), NSP.namedScratchpadAction scratchpads "calculator")

   -- Close Focused Window
   , ((alt, xK_w), spawn "close-window")

   -- Prev/Next Hidden NonEmpty Workspace
   , ((alt, xK_bracketleft), CW.moveTo CW.Prev (CW.WSIs hiddenNotNSP))
   , ((alt, xK_bracketright), CW.moveTo CW.Next (CW.WSIs hiddenNotNSP))

   -- Prev/Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((super, xK_bracketleft), sequence_ [CW.nextScreen, CW.moveTo CW.Prev (CW.WSIs hiddenNotNSP), CW.prevScreen])
   , ((super, xK_bracketright), sequence_ [CW.nextScreen, CW.moveTo CW.Next (CW.WSIs hiddenNotNSP), CW.prevScreen])

   -- Toggle to Last Workspace
   , ((super, xK_o), CW.toggleWS' ["NSP"])

   -- TMUX
   , ((alt, xK_9), spawn "tmux switchc -p")
   , ((alt, xK_0), spawn "tmux switchc -n")
   , ((alt, xK_n), spawn "tmux next-window")
   , ((alt, xK_p), spawn "tmux previous-window")
   , ((super, xK_n), spawn "tmux new-window -c $(defaultTmuxDir --get $(tmux display-message -p \"#S\"))")
   , ((alt, xK_k), spawn "tm-kill")
   , ((alt, xK_e), spawn "tm-send --action=clear")
   , ((alt, xK_q), spawn "tm-send --action=quit")
   , ((alt, xK_minus), spawn "tm-send --action='clear && cd ..'")
   , ((alt, xK_equal), spawn "tm-send --action='popd; clear'")
   , ((alt, xK_h), spawn "tm-send --action \
        \ 'clear && cd $(defaultTmuxDir --get $(tmux display-message -p \"#S\"))'")

   -- Program Launcher
   , ((alt, xK_space), spawn "dmenu_extended_run")
   , ((super, xK_space), sequence_ [DW.addWorkspace "MISC", spawn "dmenu_extended_run"])

   -- Open New Book in Okular
   , ((alt, xK_o), spawn "dmenu_books --application=okular")

   -- Toggle External Monitor
   , ((alt, xK_m), spawn "toggle_monitor")

   -- Screenshot Commands
   , ((alt, xK_Print), spawn "sshot")
   , ((super, xK_Print), spawn "receipt_sshot")

   -- Shutdown / Restart
   , ((ctrl .|. super .|. alt, xK_s),
   spawn "confirm --dmenu 'ham stop && dbox_sync && shutdown now'")
   , ((ctrl .|. super .|. alt, xK_r),
   spawn "confirm --dmenu 'ham stop && systemctl reboot -i'")

   -- Hamster Start and Stop
   , ((super, xK_KP_Delete), spawn "ham start")
   , ((super, xK_KP_Insert), spawn "ham stop")

   -- clipmenu
   , ((alt .|. super, xK_c), spawn "clipmenu")

   -- screenlock
   , ((super, xK_l), spawn "screenlock")

   -- Local WS Commands
   , ((alt, xK_f), windows $ W.focusUp)     -- Focus
   , ((super, xK_f), windows W.swapDown)    -- Shift

   -- Next Screen
   , ((alt, xK_backslash), CW.nextScreen)
   , ((alt, xK_Tab), CW.nextScreen)

   -- Swap
   , ((alt, xK_s), sequence_ [CW.swapNextScreen, spawn "removeEmptyWorkspace"])

   -- Expand or Shrink Master Area
   , ((super, xK_j), sendMessage Shrink)
   , ((super, xK_k), sendMessage Expand)

   -- Send current WS to Next Screen
   , ((super, xK_slash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"], CW.nextScreen]) -- send focus
   , ((super, xK_backslash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"]]) -- don't send focus

   -- Shift current window to MISC
   , ((super, xK_m), sequence_ [DW.addHiddenWorkspace "MISC", windows $ W.shift "MISC", DW.removeEmptyWorkspace, windows $ W.view "MISC"])

   -- Shift current window to _______
   , ((alt .|. super, xK_n), sequence_ [DW.addWorkspacePrompt myXPConfig, DW.setWorkspaceIndex 1, CW.toggleWS' ["NSP"], DW.withWorkspaceIndex W.shift 1, DW.removeEmptyWorkspace, DW.withWorkspaceIndex W.view 1])
   ]

   -- Hamster Numpad Bindings
   ++ [((super, key), spawn $ "ham start " ++ (show i))
       | (i, key) <- zip [1..9]
       [xK_KP_End,xK_KP_Down,xK_KP_Page_Down,xK_KP_Left,xK_KP_Begin,xK_KP_Right,xK_KP_Home,xK_KP_Up,xK_KP_Page_Up]
      ]

   -- Launch Applications
   ++ [((alt, key), sequence_ [DW.addWorkspace ws, (spawnOn ws $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [xK_x, xK_c, xK_z, xK_v, xK_a, xK_1, xK_2]
       ["termite -e 'tm-init Terminal'","google-chrome-stable","zathura","okular","anki","hamster","slack"]
       ["TERM","WEB","ZATH","OKULAR","ANKI","HAMSTER","SLACK"]
      ]

   -- Launch Second Applications
   ++ [((super, key), sequence_ [DW.addWorkspace ws, (spawnOn ws $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [xK_c, xK_z]
       ["google-chrome-stable", "zathura"]
       ["WEB'", "ZATH'"]
      ]

   -- Shift to WS; then Focus WS
   ++ [((super, k), sequence_ [withNthWorkspace' W.shift i, DW.removeEmptyWorkspace, withNthWorkspace' W.view i])
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

myXPConfig :: P.XPConfig
myXPConfig = def {P.position = P.Bottom}

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = smartSpacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

scratchpads = [ NSP.NS "scratchpad" scratchpad (role =? "scratchpad") 
                    (NSP.customFloating $ W.RationalRect l t w h)
              , NSP.NS "calculator" "galculator" (className =? "Galculator")
                    (NSP.customFloating $ W.RationalRect l t w h)]
            where 
                role = stringProperty "WM_WINDOW_ROLE"
                scratchpad = "termite -r scratchpad --class scratchpad -d ~/Dropbox/notes" 
                h = 0.5
                w = 0.5
                t = 0.4  -- Distance from top edge
                l = 0.25

myManageHook = composeAll
    [ manageSpawn
    , NSP.namedScratchpadManageHook scratchpads
    , className=? "Galculator"      --> doFloat
    , className=? "Pinentry"        --> doFloat]

myStartupHook = ewmhDesktopsStartup
                >> setWMName "LG3D"
                >> spawn "maintCheck"
                >> spawn "init-bg"
                >> spawn "sleep 3 && volume-xmonad"
                >> spawn "alarm-xmonad --resume"

-------------------------------- Main -----------------------------------------
main :: IO ()
main = do
    hostname <- getHostName
    xmproc <- spawnPipe ("xmobar " ++ getXmobarTemplate hostname ++ " /home/bryan/.xmobarrc")
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
          , logHook                 = DL.dynamicLogWithPP DL.xmobarPP
            { DL.ppOutput                = hPutStrLn xmproc
            , DL.ppOrder                 = \(ws:l:t:_)   -> [ws]
            , DL.ppCurrent               = DL.xmobarColor "yellow" "" . DL.wrap "[" "]"
            , DL.ppHidden                = DL.xmobarColor "white" ""
            , DL.ppHiddenNoWindows       = DL.xmobarColor "darkgrey" ""
            , DL.ppWsSep                 = "    "
            , DL.ppTitle                 = DL.xmobarColor "green"  "" . DL.shorten 40
            , DL.ppVisible               = DL.xmobarColor "yellow" ""
            , DL.ppUrgent                = DL.xmobarColor "red" "yellow"
            , DL.ppSort                  = (NSP.namedScratchpadFilterOutWorkspace .) `liftM` getSortByIndex
            } >> ewmhDesktopsLogHook <+> DL.dynamicLogXinerama
      } `additionalKeys` myAdditionalKeys
