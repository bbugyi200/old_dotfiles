-------------------------------------------------------------------------------
-------------------------------- Imports --------------------------------------
import XMonad
import XMonad.Actions.SpawnOn (spawnOn,spawnHere,manageSpawn)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhDesktopsLogHook,ewmhDesktopsStartup)
import XMonad.Hooks.ManageHelpers (doRectFloat)

import Data.Maybe (isNothing,isJust)
import Control.Monad (liftM,when)
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
  sort <- DW.getSortByOrder
  hs <- gets (map W.tag . sort . NSP.namedScratchpadFilterOutWorkspace . W.hidden . windowset)
  return (\w -> (W.tag w) `elem` hs)

-- | This is a re-implementation of DW.withNthworkspace with "skipTags"
-- added to filter out NSP.
withNthWorkspace' :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace' job wnum = do
    sort <- DW.getSortByOrder
    ws <- gets (map W.tag . sort . NSP.namedScratchpadFilterOutWorkspace . W.workspaces . windowset)
    case drop wnum ws of
        (w:_) -> windows $ job w
        []    -> return ()

xmobarTempFmt :: String -> String
xmobarTempFmt temp = "xmobar --template=\"" ++ temp ++ "\" /home/bryan/.xmobarrc"

getXmobarTemplate :: String -> String
getXmobarTemplate "athena" = "%UnsafeStdinReader% }%watson%{ %alarm%%pia%%dynnetwork%  |  %dropbox%  |  %volume%  |  %date%"
getXmobarTemplate "aphrodite" = "%UnsafeStdinReader% }%watson%{ %alarm%%pia%%dynnetwork%  |  %dropbox%  |  %battery%  |  %volume%  |  %date%"
getXmobarTemplate "secondary" = "%cpu%  |  %memory%}%KVAY%{"   -- KVAY: Mount Holly; KSMQ: Piscataway Township

removeEmptyWorkspaceAfter' f = do
    workspaceList <- gets (W.workspaces . windowset)
    let n = length $ workspaceList
    when (n > 3) $ DW.removeEmptyWorkspaceAfter f
    when (n <= 3) $ f

removeEmptyWorkspace' = do
    workspaceList <- gets (W.workspaces . windowset)
    let n = length $ workspaceList
    when (n > 3) $ DW.removeEmptyWorkspace

------------------------------- Key Bindings ----------------------------------

------- Modifier Masks (mod1Mask: alt, mod4Mask: super)
meta = mod1Mask
super = mod4Mask
ctrl = controlMask
shift = shiftMask

myAdditionalKeys = [
   -- Alarm
   ((meta, a), spawn "alarm-xmonad")

   -- clipmenu
   , ((meta, b), spawn "clipmenu")

   -- Close Focused Window
   , ((meta, w), spawn "close-window")

   -- Expand or Shrink Master Area
   , ((meta, j), sendMessage Shrink)
   , ((meta, k), sendMessage Expand)

   -- Local WS Commands
   , ((meta, f), windows $ W.focusUp)     -- Focus
   , ((meta .|. ctrl, f), windows W.swapDown)    -- Shift

   -- Next Layout
   , ((super .|. meta, xK_space), sendMessage NextLayout)  -- Toggles Fullscreen

   -- Next Screen
   , ((meta, xK_backslash), CW.nextScreen)
   , ((meta, xK_Tab), CW.nextScreen)

   -- Open New Book in Okular
   , ((meta .|. ctrl, o), spawn "dmenu_books --application=okular")

   -- Prev/Next Hidden NonEmpty Workspace
   , ((meta, xK_bracketleft), sequence_ [CW.moveTo CW.Prev (CW.WSIs hiddenNotNSP)])
   , ((meta, xK_bracketright), sequence_ [CW.moveTo CW.Next (CW.WSIs hiddenNotNSP)])

   -- Prev/Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((meta .|. ctrl, xK_bracketleft), sequence_ [CW.nextScreen, CW.moveTo CW.Prev (CW.WSIs hiddenNotNSP), CW.prevScreen])
   , ((meta .|. ctrl, xK_bracketright), sequence_ [CW.nextScreen, CW.moveTo CW.Next (CW.WSIs hiddenNotNSP), CW.prevScreen])

   -- Program Launcher
   , ((meta, xK_space), spawn "rofi -modi drun -show drun")
   , ((meta .|. ctrl, xK_space), sequence_ [DW.addWorkspace "MISC", spawn "rofi -modi drun -show drun"])

   -- Restarts XMonad
   , ((meta, r), spawn "killall xmobar; xmonad --recompile && xmonad --restart")

   -- Remove Workspaces
   , ((meta .|. super, r), DW.removeWorkspace)  -- Remove Current Workspace
   , (( super .|. shift, n), removeEmptyWorkspace') -- if Empty

   -- Screenshot Commands
   , ((meta, xK_Print), spawn "sshot")
   , ((meta .|. ctrl, xK_Print), spawn "receipt_sshot")

   -- Scratchpad
   , ((meta, xK_KP_End), NSP.namedScratchpadAction scratchpads "scratchpad")
   , ((0, xF86XK_Calculator), NSP.namedScratchpadAction scratchpads "calculator")
   , ((ctrl .|. meta, c), NSP.namedScratchpadAction scratchpads "calculator")
   , ((meta, xK_KP_Down), NSP.namedScratchpadAction scratchpads "taskwarrior")

   -- screenlock
   , ((meta, l), spawn "screenlock")

   -- Send current WS to Next Screen
   , ((ctrl .|. meta, xK_slash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"], CW.nextScreen]) -- send focus
   , ((ctrl .|. meta, xK_backslash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"]]) -- don't send focus

   -- Shift current window to MISC
   , ((meta, m), sequence_ [DW.addHiddenWorkspace "MISC", windows $ W.shift "MISC", removeEmptyWorkspaceAfter' $ windows $ W.view "MISC"])

   -- Shift current window to _______
   , ((ctrl, xK_0), sequence_ [DW.addWorkspacePrompt myXPConfig, DW.setWorkspaceIndex 1, CW.toggleWS' ["NSP"], DW.withWorkspaceIndex W.shift 1, removeEmptyWorkspaceAfter' $ DW.withWorkspaceIndex W.view 1])

   -- Shutdown / Restart
   , ((ctrl .|. meta .|. super, s),
   spawn "confirm --dmenu 'ham stop && dbox_sync && shutdown now'")
   , ((ctrl .|. meta .|. super, r),
   spawn "confirm --dmenu 'ham stop && systemctl reboot -i'")

   -- Swap
   , ((meta, s), sequence_ [removeEmptyWorkspace', CW.swapNextScreen, removeEmptyWorkspace'])
   , ((ctrl .|. meta, s), sequence_ [removeEmptyWorkspace', CW.swapNextScreen, removeEmptyWorkspace', CW.nextScreen])

   -- taskwarrior
   , ((ctrl .|. meta, t), spawn "rofi -dmenu -p 'Inbox' | sed \"s/\\([\\\'\\\"]\\)/\\\\\\\\\\1/g\" | xargs task add +inbox | tail -1 | xargs -I _ notify-send -u low _")

   -- Toggle to Last Workspace
   , ((meta, o), CW.toggleWS' ["NSP"])

   -- Toggle External Monitor
   , ((ctrl .|. meta, m), spawn "toggle_monitor && sleep 1 && killall xmobar; xmonad --restart")

   -- Toggle PIA
   , ((ctrl .|. meta, p), spawn "PIA")

   -- TMUX
   , ((meta, xK_9), spawn "tmux switchc -p")
   , ((meta, xK_0), spawn "tmux switchc -n")
   , ((meta, n), spawn "tmux next-window")
   , ((meta, p), spawn "tmux previous-window")
   , ((meta, k), spawn "tm-kill")
   , ((meta, e), spawn "tm-send --action=clear")
   , ((meta, q), spawn "tm-send --action=quit")
   , ((meta, xK_minus), spawn "tm-send --action='pushu && popd'")
   , ((meta, xK_equal), spawn "tm-send --action='cd $(popu)'")
   , ((meta, h), spawn "tm-send --action \
        \ 'clear && cd $(defaultTmuxDir --get $(tmux display-message -p \"#S\"))'")
   ]

   -- Launch Applications
   ++ [((meta, key), sequence_ [DW.addWorkspace ws, (spawnHere $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [x, c, z, v, xK_1, xK_2]
       [myTerminal,"qutebrowser","zathura","okular","anki","slack"]
       ["TERM","WEB","ZATH","OKULAR","ANKI","SLACK"]
      ]

   -- Launch Second Applications
   ++ [((ctrl .|. meta, key), sequence_ [CW.nextScreen, DW.addWorkspace ws, (spawnOn ws $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [c, x, z, v]
       ["qutebrowser", myTerminal, "zathura", "zathura"]
       ["WEB'", "TERM'", "ZATH'", "ZATH"]
      ]

   -- Shift to WS; then Focus WS
   ++ [((ctrl, k), sequence_ [withNthWorkspace' W.shift i, withNthWorkspace' W.view i])
       | (i, k) <- zip [0..8] $ [xK_1 .. xK_9]
      ]
   where
       a = xK_a; b = xK_b; c = xK_c; d = xK_d; e = xK_e; f = xK_f
       g = xK_g; h = xK_h; i = xK_i; j = xK_j; k = xK_k; l = xK_l
       m = xK_m; n = xK_n; o = xK_o; p = xK_p; q = xK_q; r = xK_r
       s = xK_s; t = xK_t; u = xK_u; v = xK_v; w = xK_w; x = xK_x
       y = xK_y; z = xK_z

-------------------------------- Misc Configs ---------------------------------
myTerminal = "urxvt -e zsh -c 'tm-init Terminal'"

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
myWorkspaces = ["TERM","WEB","NSP"]

myXPConfig :: P.XPConfig
myXPConfig = def {P.position = P.Bottom}

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = smartSpacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

scratchpads = [ NSP.NS "scratchpad" scratchpad (appName =? "scratchpad") 
                    (NSP.customFloating $ W.RationalRect l t w h)
              , NSP.NS "calculator" "galculator" (className =? "Galculator")
                    (NSP.customFloating $ W.RationalRect l t w h)
              , NSP.NS "taskwarrior" taskwarrior (appName =? "taskwarrior")
                    (NSP.customFloating $ W.RationalRect 0.1 0.2 0.75 0.75)]
            where 
                role = stringProperty "WM_WINDOW_ROLE"
                scratchpad = "urxvt -name scratchpad -cd ~/Dropbox/notes/misc" 
                taskwarrior = "urxvt -name taskwarrior -e zsh -c 'task next +READY; zsh'"
                h = 0.5
                w = 0.5
                t = 0.4  -- Distance from top edge
                l = 0.25

myManageHook = composeAll
    [ manageSpawn
    , NSP.namedScratchpadManageHook scratchpads
    , className=? "Galculator"      --> doFloat
    , className=? "Peek"            --> doFloat
    , className=? "Pinentry"        --> doFloat
    , appName=? "qute-editor"     --> doRectFloat (W.RationalRect x y w h)]
    where
        x = 0.3
        y = 0.4
        w = 0.3
        h = 0.15

myStartupHook = ewmhDesktopsStartup
                >> setWMName "LG3D"
                >> spawn "maintCheck"
                >> spawn "init-bg"
                >> spawn "sleep 3 && volume-xmonad"
                >> spawn "alarm-xmonad --resume"
                >> spawn ("[[ $(x11screens) -ge 2 ]] && " ++ (xmobarTempFmt $ getXmobarTemplate "secondary") ++ " --screen=1")

-------------------------------- Main -----------------------------------------
main :: IO ()
main = do
    hostname <- getHostName
    xmproc <- spawnPipe (xmobarTempFmt $ getXmobarTemplate hostname)
    xmonad $ ewmh desktopConfig
        {
            terminal                = myTerminal
          , modMask                 = meta
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
            , DL.ppSort                  = (NSP.namedScratchpadFilterOutWorkspace .) `liftM` DW.getSortByOrder
            } >> ewmhDesktopsLogHook <+> DL.dynamicLogXinerama
      } `additionalKeys` myAdditionalKeys
